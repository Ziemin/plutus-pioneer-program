{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Week03.Homework1 where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract hiding (when)
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import Text.Printf (printf)
import qualified Prelude as P

data VestingDatum = VestingDatum
  { beneficiary1 :: PubKeyHash,
    beneficiary2 :: PubKeyHash,
    deadline :: Slot
  }
  deriving (Show)

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINEABLE mkValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx =
  traceIfFalse "No beneficiary is eligible" (checkBeneficiary1 || checkBeneficiary2)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txValidRange :: SlotRange
    txValidRange = txInfoValidRange info

    hasSigned :: (VestingDatum -> PubKeyHash) -> Bool
    hasSigned getBeneficiary = getBeneficiary dat `elem` txInfoSignatories info

    checkBeneficiary :: (VestingDatum -> PubKeyHash) -> Interval Slot -> Bool
    checkBeneficiary getBeneficiary validityInterval =
      hasSigned getBeneficiary
        -- There should not `not` before isEmpty, but it's not there due to the bug in the lib
        && isEmpty (validityInterval `Ledger.intersection` txValidRange)

    checkBeneficiary1 :: Bool
    checkBeneficiary1 = checkBeneficiary beneficiary1 validityInterval
      where
        validityInterval = to (deadline dat)

    checkBeneficiary2 :: Bool
    checkBeneficiary2 = checkBeneficiary beneficiary2 validityInterval
      where
        validityInterval = Interval (strictLowerBound (deadline dat)) (UpperBound PosInf True)

data Vesting

instance Scripts.ScriptType Vesting where
  type DatumType Vesting = VestingDatum
  type RedeemerType Vesting = ()

inst :: Scripts.ScriptInstance Vesting
inst =
  Scripts.validator @Vesting
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveParams = GiveParams
  { gpBeneficiary :: !PubKeyHash,
    gpDeadline :: !Slot,
    gpAmount :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
  BlockchainActions
    .\/ Endpoint "give" GiveParams
    .\/ Endpoint "grab" ()

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give gp = do
  pkh <- pubKeyHash <$> ownPubKey
  let dat =
        VestingDatum
          { beneficiary1 = gpBeneficiary gp,
            beneficiary2 = pkh,
            deadline = gpDeadline gp
          }
      tx = mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
  ledgerTx <- submitTxConstraints inst tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $
    printf
      "made a gift of %d lovelace to %s with deadline %s"
      (gpAmount gp)
      (show $ gpBeneficiary gp)
      (show $ gpDeadline gp)

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
grab = do
  now <- currentSlot
  pkh <- pubKeyHash <$> ownPubKey
  utxos <- utxoAt scrAddress
  let utxos1 = Map.filter (isSuitable $ \dat -> beneficiary1 dat == pkh && now <= deadline dat) utxos
      utxos2 = Map.filter (isSuitable $ \dat -> beneficiary2 dat == pkh && now > deadline dat) utxos
  logInfo @String $ printf "found %d gift(s) to grab" (Map.size utxos1 P.+ Map.size utxos2)
  unless (Map.null utxos1) $ do
    let orefs = fst <$> Map.toList utxos1
        lookups =
          Constraints.unspentOutputs utxos1
            P.<> Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx =
          mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs]
            P.<> mustValidateIn (to now)
    void $ submitTxConstraintsWith @Void lookups tx
  unless (Map.null utxos2) $ do
    let orefs = fst <$> Map.toList utxos2
        lookups =
          Constraints.unspentOutputs utxos2
            P.<> Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx =
          mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs]
            P.<> mustValidateIn (from now)
    void $ submitTxConstraintsWith @Void lookups tx
  where
    isSuitable :: (VestingDatum -> Bool) -> TxOutTx -> Bool
    isSuitable p o = case txOutDatumHash $ txOutTxOut o of
      Nothing -> False
      Just h -> case Map.lookup h $ txData $ txOutTxTx o of
        Nothing -> False
        Just (Datum e) -> maybe False p $ PlutusTx.fromData e

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >> grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
