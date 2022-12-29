import Playground.Contract
import           Control.Monad             (void)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)
import           Language.Plutus.Contract
import qualified Language.PlutusTx         as PlutusTx
import           Language.PlutusTx.Prelude
import           Ledger
import qualified Ledger.Ada                as Ada
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Schema
import           Wallet.Emulator.Wallet

data SmartContractData =
    SmartContractData
        { recipient :: PubKeyHash -- Destinatario de los fondos
        , amount     :: Ada -- ^ Cantidad de Ada (moneda)
        }
    deriving stock (Show, Generic)

PlutusTx.makeIsData ''SmartContractData
PlutusTx.makeLift ''SmartContractData

validateSmartContract :: SmartContractData -> () -> ValidatorCtx -> Bool
validateSmartContract SmartContractData{recipient, amount} _ ValidatorCtx{valCtxTxInfo} =
    Ada.fromValue (valuePaidTo valCtxTxInfo recipient) == amount

data SmartContract
instance Scripts.ScriptType SmartContract where
    type instance RedeemerType SmartContract = ()
    type instance DatumType SmartContract = SmartContractData

smartContractInstance :: Scripts.ScriptInstance SmartContract
smartContractInstance = Scripts.validator @SmartContract
    $$(PlutusTx.compile [|| validateSmartContract ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @SmartContractData @()

data LockArgs =
        LockArgs
            { recipientWallet :: Wallet
            , totalAda         :: Ada
            }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type SmartContractSchema =
    BlockchainActions
        .\/ Endpoint "lock" LockArgs
        .\/ Endpoint "unlock" LockArgs

lock :: Contract SmartContractSchema T.Text LockArgs
lock = endpoint @"lock"

unlock :: Contract SmartContractSchema T.Text LockArgs
unlock = endpoint @"unlock"

mkSmartContractData :: LockArgs -> SmartContractData
mkSmartContractData LockArgs{recipientWallet, totalAda} =
    let convert :: Wallet -> PubKeyHash
        convert = pubKeyHash . walletPubKey
    in
    SmartContractData
        { recipient = convert recipientWallet
        , amount = totalAda
        }

lockFunds :: SmartContractData -> Contract SmartContractSchema T.Text ()
lockFunds s@SmartContractData{amount} = do
    logInfo $ "Locking " <> show amount
    let tx = Constraints.mustPayToTheScript s (Ada.toValue amount)
    void $ submitTxConstraints smartContractInstance tx

unlockFunds :: SmartContractData -> Contract SmartContractSchema T.Text ()
unlockFunds SmartContractData{recipient, amount} = do
    let contractAddress = (Ledger.scriptAddress (Scripts.validatorScript smartContractInstance))
    utxos <- utxoAt contractAddress
    let tx =
            collectFromScript utxos ()
            <> Constraints.mustPayToPubKey recipient (Ada.toValue $ amount)
    void $ submitTxConstraintsSpending smartContractInstance utxos tx

endpoints :: Contract SmartContractSchema T.Text ()
endpoints = (lock >>= lockFunds . mkSmartContractData) `select` (unlock >>= unlockFunds . mkSmartContractData)

mkSchemaDefinitions ''SmartContractSchema
$(mkKnownCurrencies [])
