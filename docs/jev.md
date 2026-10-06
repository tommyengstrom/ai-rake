# Jev decisions

`TypeSafe.Jev` is an independent Effectful client for TypeSafe AI's Jev models. Its
modules do not depend on `Rake` and can be extracted into a separate package.

Build a `Batch a` using `choice`, `score`, `noul`, and ordinary applicative
composition. `assess` submits every question in the batch against the same state
in one request, then reconstructs your result `a`. Questions in a batch are
independent; an answer-dependent follow-up uses another `assess` call.

## Command-line client

`rake-decide` supports Jev without writing Haskell:

```bash
export TYPESAFE_API_KEY='your-key'
rake-decide choice Billing Support Sales -c message.txt
cat message.txt | rake-decide choice Billing Support Sales
```

Choices are positional arguments. Use `NAME=DESCRIPTION` to describe a choice:

```bash
rake-decide choice 'approve=Ready to merge' 'reject=Needs changes' \
  --question 'Should this change be merged?' \
  --context diff.txt review-notes.txt
```

`-c` and `--context` consume one or more filenames up to the next option. Put
choices before `-c`. Without `-c`, context comes from stdin; `-c -` also explicitly
selects stdin. A single source is sent as text. Multiple files are sent together
as an array of objects containing `file` and `content`, preserving their order.
Files must contain UTF-8 text. Add `--json` to parse each source as a JSON string,
object, or array instead.

Score and Noul are also supported:

```bash
rake-decide score Low Medium High -q 'How urgent?' -c message.txt
rake-decide noul 'Is this spam?' --true 'Unsolicited advertising' -c message.txt
```

Results are JSON on stdout, with the answer under `answers.result`, the resolved
`model`, and `usage.input_tokens` / `usage.output_tokens`. Choice answers include
`choice`, `confidence`, and `probabilities`. To extract just the chosen name:

```bash
cat message.txt | rake-decide choice Billing Support Sales | jq -r '.answers.result.choice'
```

Errors go to stderr and exit nonzero. The default and currently supported model
is `typesafe/jev-latest`; select it explicitly with `-m typesafe/jev-latest`,
`--model=typesafe/jev-latest`, or the unique bare name `-m jev-latest`. Selection
first tries an exact qualified match, then a unique model-name match (ignoring
any unmatched provider prefix). Unknown or ambiguous models fail before context
is read. The library still accepts arbitrary model IDs.

`--base-url` and `--timeout` override the library defaults. `rake-decide --help`
lists all options and models and needs no credentials. From a checkout, build
through the managed process-compose build job, then use
`"$(cabal list-bin -O0 exe:rake-decide)" choice ...` to run the built executable.

## Complete example

This example is also compiled and exercised against a local HTTP fixture in
`test/TypeSafe/JevExample.hs` and `test/TypeSafe/JevSpec.hs`.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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
```

Install the HTTP interpreter around the application action:

```haskell
{-# LANGUAGE TypeApplications #-}

import Data.Text qualified as Text
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import System.Environment (getEnv)

main :: IO ()
main = do
    apiKey <- Text.pack <$> getEnv "TYPESAFE_API_KEY"
    result <- runEff . runErrorNoCallStack @JevError $
        runJev (defaultJevSettings apiKey) runExample
    print result
```

## Inputs and typed choices

`Content` accepts text (`TextContent`, or an overloaded string), JSON objects
(`ObjectContent`), and arrays (`ArrayContent`). Objects and arrays may contain
arbitrary nested JSON values. The same type is used for instructions and
criteria; `Maybe Content` represents optional descriptions or instructions.

Each `ChoiceOption a` holds a model-visible name, an optional description, and an
arbitrary local value `a`. `shownOption` uses `Show` for the name; construct
`ChoiceOption` directly to customize it. Both the name and description influence
evaluation. Names must be unique within a question. Duplicate names fail locally
with `InvalidRequest`.

Only supplied values are offered. The example's `Department` includes `Sales`,
but its question offers only `Billing` and `TechnicalSupport`. The library maps
returned names back to those original values; no `Read`, `Enum`, `Bounded`, `Eq`,
`Ord`, or JSON instances are required. `Show` is needed only by `shownOption`.

Choice options and score levels use `NonEmpty`. Model-specific cardinality and
token limits are handled by the service. The [live API schema](https://api.typesafe.ai/openapi.json)
is the wire contract; it accepts a one-level score rubric even though the prose
documentation recommends at least two levels.

## Answers and metadata

- `ChoiceAssessment a` contains the selected `ChoiceOption a`, all option
  probabilities in the supplied order, and confidence.
- `ScoreAssessment` contains the fractional score, the returned rubric
  descriptions with probabilities in numeric level order, and confidence.
- `NoulAssessment` contains the probability that the answer is yes. It has no
  separate confidence and is not automatically converted to `Bool`.
- `Probability` and `Confidence` have private constructors and validated JSON
  decoders. Use `probabilityValue` and `confidenceValue` to read their numbers.
- `Response a` contains your `answers :: a` and `metadata :: Maybe CallMetadata`.
  A real request returns the resolved model version and input/output token counts.

`pure x` and `traverse noul []` evaluate locally, make no HTTP request, and return
`metadata = Nothing`. `fmap` and `<*>` preserve all questions in a nonempty batch,
even when the final result does not use every answer. There is no `Monad Batch`
instance; use applicative composition to describe each request.

Returned question IDs, answer kinds, choice labels, and score indexes are checked
against the request. Invalid or incomplete responses fail the whole assessment
with `InvalidResponse`. Probabilities and confidence must be finite and within
0–1. Scores must be finite and within the requested rubric. Server scores and
confidence are preserved; the library does not recalculate them or require exact
floating-point probability sums.

## Interpreter settings and errors

`defaultJevSettings apiKey` uses `https://api.typesafe.ai`, model `jev-latest`, and a
30-second timeout. Override `baseUrl`, `model`, or `requestTimeout` through record
updates. A base URL can contain a path prefix; `/v1/systemone` is appended. It must
not contain a query string. API keys containing control characters fail locally.
Model names remain open `Text` values, so pinned
versions and future aliases need no library update.

`runJev` uses one HTTP manager for its lifetime. Each nonempty assessment sends
one POST, with no automatic retries, redirects, or splitting. The timeout bounds
the complete HTTP operation. Asynchronous cancellation is preserved.

Failures use the independent `JevError` type: `InvalidRequest`,
`TransportFailure`, `HttpFailure`, `InvalidResponse`, and `RequestTimedOut`.
`HttpFailure` preserves the status, raw response body, and optional
`x-typesafe-request-id`. Transport errors do not render the authenticated request.
The interpreter does not log request bodies or read environment variables.

See the official [API reference](https://docs.typesafe.ai/api),
[primitives](https://docs.typesafe.ai/primitives), and
[model documentation](https://docs.typesafe.ai/models).
