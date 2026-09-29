module TypeSafe.JevExample
    ( Department (..)
    , Tone (..)
    , Report (..)
    , customerState
    , departmentQuestion
    , toneQuestion
    , severityQuestion
    , humanQuestion
    , reportQuestions
    , runExample
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Effectful (Eff, (:>))
import TypeSafe.Jev
import Prelude

data Department = Billing | TechnicalSupport | Sales
    deriving stock (Show, Eq)

data Tone = Calm | Frustrated | Angry
    deriving stock (Show, Eq)

data Report = Report
    { department :: ChoiceAssessment Department
    , tone :: ChoiceAssessment Tone
    , severity :: ScoreAssessment
    , wantsHuman :: NoulAssessment
    }
    deriving stock (Show, Eq)

customerState :: Content
customerState =
    "I was charged twice for my subscription. \
    \The product still works, but this is the third time \
    \I've contacted you. Please let me speak to a person."

departmentQuestion :: ChoiceQuestion Department
departmentQuestion =
    ChoiceQuestion
        { instructions = Just "Which department should handle this?"
        , options =
            shownOption Billing (Just "Payments, invoices, subscriptions, and refunds")
                :| [shownOption TechnicalSupport (Just "Bugs, outages, and problems using the product")]
        }

toneQuestion :: ChoiceQuestion Tone
toneQuestion =
    ChoiceQuestion
        { instructions = Just "What is the customer's tone?"
        , options =
            shownOption Calm (Just "Neutral, patient, or satisfied")
                :| [ shownOption Frustrated (Just "Dissatisfied or impatient, but civil")
                   , shownOption Angry (Just "Hostile or strongly confrontational")
                   ]
        }

severityQuestion :: ScoreQuestion
severityQuestion =
    ScoreQuestion
        { instructions = Just "How severe is the customer's problem?"
        , levels =
            "Minor inconvenience with no financial or functional impact"
                :| [ "Financial impact or impaired functionality, but the product remains usable"
                   , "The customer cannot use the product at all"
                   ]
        }

humanQuestion :: NoulQuestion
humanQuestion =
    NoulQuestion
        { instructions = Just "Is the customer explicitly asking to speak to a person?"
        , criteria =
            Just
                NoulCriteria
                    { whenTrue = Just "Explicitly requests a human, agent, or representative"
                    , whenFalse = Just "Asks for help without requesting a person"
                    }
        }

reportQuestions :: Batch Report
reportQuestions =
    Report
        <$> choice departmentQuestion
        <*> choice toneQuestion
        <*> score severityQuestion
        <*> noul humanQuestion

runExample :: Jev :> es => Eff es (Response Report)
runExample = assess customerState reportQuestions
