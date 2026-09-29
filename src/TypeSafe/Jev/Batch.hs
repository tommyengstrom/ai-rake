module TypeSafe.Jev.Batch
    ( Batch
    , choice
    , score
    , noul
    , CompiledBatch (..)
    , compileBatch
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Data.Text qualified as Text
import TypeSafe.Jev.Error
import TypeSafe.Jev.Types
import TypeSafe.Jev.Wire qualified as Wire
import Prelude

data Batch a where
    PureBatch :: a -> Batch a
    ApplyBatch :: Batch (a -> b) -> Batch a -> Batch b
    ChoiceBatch :: ChoiceQuestion a -> Batch (ChoiceAssessment a)
    ScoreBatch :: ScoreQuestion -> Batch ScoreAssessment
    NoulBatch :: NoulQuestion -> Batch NoulAssessment

instance Functor Batch where
    fmap f = ApplyBatch (PureBatch f)

instance Applicative Batch where
    pure = PureBatch
    (<*>) = ApplyBatch

choice :: ChoiceQuestion a -> Batch (ChoiceAssessment a)
choice = ChoiceBatch

score :: ScoreQuestion -> Batch ScoreAssessment
score = ScoreBatch

noul :: NoulQuestion -> Batch NoulAssessment
noul = NoulBatch

type AnswerDecoder a = Map Text Wire.WireAnswer -> Either JevError a

data CompiledBatch a = CompiledBatch
    { questions :: Map Text Wire.WireQuestion
    , decodeAnswers :: AnswerDecoder a
    }

compileBatch :: Batch a -> Either JevError (CompiledBatch a)
compileBatch batch = do
    (_, questionBuilder, decoder) <- compileFrom 0 batch
    let questions :: Map Text Wire.WireQuestion
        questions = Map.fromList (appEndo questionBuilder [])
    pure
        CompiledBatch
            { questions
            , decodeAnswers = \answers -> do
                requireSameKeys "Question IDs" questions answers
                decoder answers
            }

compileFrom
    :: Int
    -> Batch a
    -> Either JevError (Int, Endo [(Text, Wire.WireQuestion)], AnswerDecoder a)
compileFrom nextId = \case
    PureBatch value -> pure (nextId, mempty, const (Right value))
    ApplyBatch function argument -> do
        (afterFunction, functionQuestions, decodeFunction) <- compileFrom nextId function
        (afterArgument, argumentQuestions, decodeArgument) <- compileFrom afterFunction argument
        pure
            ( afterArgument
            , functionQuestions <> argumentQuestions
            , \answers -> decodeFunction answers <*> decodeArgument answers
            )
    ChoiceBatch (ChoiceQuestion{instructions, options} :: ChoiceQuestion option) -> do
        let optionMap :: Map Text (ChoiceOption option)
            optionMap = Map.fromList [(name, option) | option@ChoiceOption{name} <- NonEmpty.toList options]
        if Map.size optionMap /= NonEmpty.length options
            then Left (InvalidRequest (questionId <> ": duplicate choice option names"))
            else
                pure
                    ( nextId + 1
                    , Endo
                        ( ( questionId
                          , Wire.Choice instructions (Map.map (\ChoiceOption{description} -> description) optionMap)
                          )
                            :
                        )
                    , decodeChoice questionId options
                    )
    ScoreBatch ScoreQuestion{instructions, levels} ->
        pure
            ( nextId + 1
            , Endo ((questionId, Wire.Score instructions (NonEmpty.toList levels)) :)
            , decodeScore questionId levels
            )
    NoulBatch NoulQuestion{instructions, criteria} ->
        pure
            ( nextId + 1
            , Endo
                ( ( questionId
                  , Wire.Noul
                        instructions
                        ( fmap
                            (\NoulCriteria{whenTrue, whenFalse} -> Wire.WireNoulCriteria whenTrue whenFalse)
                            criteria
                        )
                  )
                    :
                )
            , \answers -> do
                answer <- lookupAnswer questionId answers
                case answer of
                    Wire.NoulAnswer probability -> Right (NoulAssessment probability)
                    Wire.ChoiceAnswer{} -> invalid questionId "Expected a noul answer"
                    Wire.ScoreAnswer{} -> invalid questionId "Expected a noul answer"
            )
  where
    questionId :: Text
    questionId = "q" <> Text.pack (show nextId)

decodeChoice
    :: forall a. Text -> NonEmpty (ChoiceOption a) -> AnswerDecoder (ChoiceAssessment a)
decodeChoice questionId options answers = do
    answer <- lookupAnswer questionId answers
    case answer of
        Wire.ChoiceAnswer{choice = selectedName, confidence, probabilities} -> do
            requireSameKeys (questionId <> " choice probabilities") optionMap probabilities
            chosen <- lookupField questionId selectedName optionMap
            distribution <-
                traverse
                    (\option@ChoiceOption{name} -> (option,) <$> lookupField questionId name probabilities)
                    options
            pure ChoiceAssessment{chosen, probabilities = distribution, confidence}
        Wire.ScoreAnswer{} -> invalid questionId "Expected a choice answer"
        Wire.NoulAnswer{} -> invalid questionId "Expected a choice answer"
  where
    optionMap :: Map Text (ChoiceOption a)
    optionMap = Map.fromList [(name, option) | option@ChoiceOption{name} <- NonEmpty.toList options]

decodeScore :: Text -> NonEmpty Content -> AnswerDecoder ScoreAssessment
decodeScore questionId requestedLevels answers = do
    answer <- lookupAnswer questionId answers
    case answer of
        Wire.ScoreAnswer{score = scoreValue, legend, confidence, probabilities} -> do
            requireSameKeys (questionId <> " score probabilities") levelMap probabilities
            requireSameKeys (questionId <> " score legend") levelMap legend
            if isNaN scoreValue
                || isInfinite scoreValue
                || scoreValue < 0
                || scoreValue > fromIntegral (NonEmpty.length requestedLevels - 1)
                then invalid questionId "Score is outside the requested rubric"
                else do
                    levels <-
                        traverse
                            ( \index ->
                                (,) <$> lookupField questionId index legend <*> lookupField questionId index probabilities
                            )
                            indexes
                    pure ScoreAssessment{scoreValue, levels, confidence}
        Wire.ChoiceAnswer{} -> invalid questionId "Expected a score answer"
        Wire.NoulAnswer{} -> invalid questionId "Expected a score answer"
  where
    indexes :: NonEmpty Text
    indexes = fmap (Text.pack . show) (0 :| [1 .. NonEmpty.length requestedLevels - 1])

    levelMap :: Map Text Content
    levelMap = Map.fromList (NonEmpty.toList (NonEmpty.zip indexes requestedLevels))

lookupAnswer :: Text -> Map Text Wire.WireAnswer -> Either JevError Wire.WireAnswer
lookupAnswer questionId = lookupField questionId questionId

lookupField :: Text -> Text -> Map Text a -> Either JevError a
lookupField context key fields =
    maybe (invalid context ("Unknown or missing key: " <> key)) Right (Map.lookup key fields)

requireSameKeys :: Text -> Map Text a -> Map Text b -> Either JevError ()
requireSameKeys context expected actual
    | Map.keysSet expected == Map.keysSet actual = Right ()
    | otherwise = invalid context "Response keys do not match the request"

invalid :: Text -> Text -> Either JevError a
invalid context message = Left (InvalidResponse (context <> ": " <> message))
