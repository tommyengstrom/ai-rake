# Jev decisions

`TypeSafe.Jev` is an independent Effectful client for TypeSafe AI's Jev models. Its
modules live in the independent `airake-jev` package and do not depend on `Rake`.

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
choices before `-c`. Without images or `-c`, context comes from stdin; `-c -` explicitly
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

Errors go to stderr and exit nonzero. The default model is `typesafe/jev-latest`.
Also available are `typesafe/jev-1.14.0`, `typesafe/jev-1.13.0`, and
`typesafe/jev-preview`; use `-m typesafe/jev-preview`, `--model=jev-preview`, or
another listed model to select it. Selection
first tries an exact qualified match, then a unique model-name match (ignoring
any unmatched provider prefix). Unknown or ambiguous models fail before context
is read. The library still accepts arbitrary model IDs.

Jev 1.14 includes improved multilingual support without a language flag. The
default follows the `jev-latest` alias; use `-m jev-1.14.0` to pin that version.
Aliases can change as TypeSafe releases new models.
For example, `rake-decide noul 'Är kunden missnöjd?' -c meddelande.txt` uses the
same API as an English question. See the [release notes](https://typesafe.ai/blog/jev-1-14).

`--base-url` and `--timeout` override the library defaults. `rake-decide --help`
lists all options and models and needs no credentials. From a checkout, build
through the managed process-compose build job, then use
`"$(cabal list-bin -O0 exe:rake-decide)" choice ...` to run the built executable.

### Images in the CLI

Attach local PNG/JPEG files with repeatable `--image [NAME=]FILE` options:

```bash
rake-decide noul 'Is an error visible in images.screen?' --image screen=screenshot.png
rake-decide choice cat dog -q 'Which animal is in images.image1?' --image photo.jpg
cat notes.txt | rake-decide noul 'Does images.photo match context?' --image photo=photo.png -c -
rake-decide noul 'Are images.before and images.after different?' \
  --image before=before.png --image after=after.jpg --json -c details.json
```

Unnamed images use their argument position: `image1`, `image2`, and so on.
Names must be nonblank and unique, including generated names. Named arguments
split at the first `=`, so `--image photo=folder/a=b.png` works. Prefix paths
starting with `-` with `./`; binary image stdin is unsupported.

With images, state contains an `images` object and, when `-c` is supplied, a
`context` field. Image-only requests do not read stdin; use `-c -` for piped text.
Multiple context files retain their existing filename/content array under
`context`, and `--json` applies only to context files. The CLI detects PNG/JPEG
from file bytes, generates UUID asset references, and enables the experimental
image header automatically.

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

## Images in the library

`assessWithAssets :: Jev :> es => Map Text ImageAsset -> Content -> Batch a -> Eff es (Response a)`
adds per-call images. `ImageAsset` holds an `ImageFormat` (`PNG` or `JPEG`) and raw
`ByteString` bytes. The library does not read files; load them at your IO boundary:

```haskell
import Data.Aeson (Value (String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID

-- Inside the IO main from the example above:
photoBytes <- BS.readFile "photo.png"
reference <- UUID.toText <$> UUID.nextRandom
let assets = Map.singleton reference (ImageAsset PNG photoBytes)
    state = ObjectContent (KeyMap.singleton "photo" (String reference))
    question = NoulQuestion (Just "Finns det en katt i `photo`?") Nothing
result <- runEff . runErrorNoCallStack @JevError $
    runJev (defaultJevSettings apiKey) (assessWithAssets assets state (noul question))
print result
```

Asset map keys are references. Place them in string values anywhere in state,
instructions, or Choice/Score/Noul criteria, including nested content. Every
asset must be referenced. Use fresh UUIDs to avoid accidental substring matches;
object keys are not substituted. Reusing a reference uploads the file once, but
each occurrence adds image input. Returned Score legends retain reference strings.

Nonempty assets use multipart uploads and automatically send
`TypeSafe-Danger-Zone-Images: true`, on both Jev 1.13 and 1.14. Empty assets use
ordinary JSON; `assess` is equivalent to `assessWithAssets Map.empty`. Empty
batches make no request even when assets are supplied. Custom interpreters
matching the exported effect constructor must handle `AssessWithAssets assets state batch`.

References must be nonblank, at most 1,024 UTF-8 bytes each and 65,536 bytes
combined. Empty image files and files over 16 MiB fail locally with `InvalidRequest`.
The service validates image contents: still PNG/JPEG, 280–1568 pixels per side,
and separate 32 MiB limits on the body, processed images, and expanded request.
Prepare upright images at the required size; this client does not resize,
convert, or download them. The API strips EXIF without applying orientation.
Invalid image content or references fail the whole assessment through `HttpFailure`.
See the [experimental image documentation](https://docs.typesafe.ai/images).

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
