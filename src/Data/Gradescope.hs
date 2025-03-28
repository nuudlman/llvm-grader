{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Gradescope 
    ( LeaderboardEntry(..)
    , TestCase(..)
    , SubmissionResults(..)
    , defaultResults
    ) where


import Data.Aeson
import Data.Text (Text)


-- | Represents a single entry in the leaderboard array.
data LeaderboardEntry = LeaderboardEntry
    { lbName  :: Text   -- Corresponds to "name", required
    , lbValue :: Value  -- Corresponds to "value", required. Using Aeson's 'Value' type
                        -- because the example shows both numbers and strings.
    , lbOrder :: Maybe Text -- Corresponds to "order", optional (e.g., "asc")
    } deriving (Show, Eq)

-- | Represents a single test case within the 'tests' array.
data TestCase = TestCase
    { tcScore         :: Maybe Double -- Corresponds to "score", optional
    , tcMaxScore      :: Maybe Double -- Corresponds to "max_score", optional
    , tcStatus        :: Maybe Text   -- Corresponds to "status", optional (e.g., "passed", "failed")
    , tcName          :: Maybe Text   -- Corresponds to "name", optional
    , tcNameFormat    :: Maybe Text   -- Corresponds to "name_format", optional (e.g., "text", "html", "md")
    , tcNumber        :: Maybe Text   -- Corresponds to "number", optional (string type as per example "1.1")
    , tcOutput        :: Maybe Text   -- Corresponds to "output", optional
    , tcOutputFormat  :: Maybe Text   -- Corresponds to "output_format", optional (e.g., "text", "html", "md")
    , tcTags          :: Maybe [Text] -- Corresponds to "tags", optional
    , tcVisibility    :: Maybe Text   -- Corresponds to "visibility", optional (e.g., "visible", "hidden", "after_due_date")
    , tcExtraData     :: Maybe Value  -- Corresponds to "extra_data", optional (arbitrary JSON object)
    } deriving (Show, Eq)

-- | Represents the top-level submission results structure.
data SubmissionResults = SubmissionResults
    { subScore            :: Maybe Double -- Corresponds to "score", optional
    , subExecutionTime    :: Maybe Int    -- Corresponds to "execution_time", optional (seconds)
    , subOutput           :: Maybe Text   -- Corresponds to "output", optional
    , subOutputFormat     :: Maybe Text   -- Corresponds to "output_format", optional (e.g., "simple_format", "md")
    , subTestOutputFormat :: Maybe Text   -- Corresponds to "test_output_format", optional (default for tests)
    , subTestNameFormat   :: Maybe Text   -- Corresponds to "test_name_format", optional (default for tests)
    , subVisibility       :: Maybe Text   -- Corresponds to "visibility", optional (e.g., "visible", "hidden", "after_due_date")
    , subStdoutVisibility :: Maybe Text   -- Corresponds to "stdout_visibility", optional (e.g., "visible", "hidden")
    , subExtraData        :: Maybe Value  -- Corresponds to "extra_data", optional (arbitrary JSON object)
    , subTests            :: Maybe [TestCase] -- Corresponds to "tests", optional
    , subLeaderboard      :: Maybe [LeaderboardEntry] -- Corresponds to "leaderboard", optional
    } deriving (Show, Eq)


maybePair :: (KeyValue e a, ToJSON v) => Key -> Maybe v -> [a]
maybePair key maybeValue = maybe [] (\value -> [key .= value]) maybeValue

instance ToJSON LeaderboardEntry where
    toJSON LeaderboardEntry{..} =
        object $ ["name" .= lbName, "value" .= lbValue] ++ maybePair "order" lbOrder

instance ToJSON TestCase where
    toJSON TestCase{..} =
        object $ 
            maybePair "score"         tcScore ++
            maybePair "max_score"     tcMaxScore ++
            maybePair "status"        tcStatus ++
            maybePair "name"          tcName ++
            maybePair "name_format"   tcNameFormat ++
            maybePair "number"        tcNumber ++
            maybePair "output"        tcOutput ++
            maybePair "output_format" tcOutputFormat ++
            maybePair "tags"          tcTags ++
            maybePair "visibility"    tcVisibility ++
            maybePair "extra_data"    tcExtraData


instance ToJSON SubmissionResults where
    toJSON SubmissionResults{..} =
        object $
            maybePair "score"             subScore ++
            maybePair "execution_time"    subExecutionTime ++
            maybePair "output"            subOutput ++
            maybePair "output_format"     subOutputFormat ++
            maybePair "test_output_format" subTestOutputFormat ++
            maybePair "test_name_format"  subTestNameFormat ++
            maybePair "visibility"        subVisibility ++
            maybePair "stdout_visibility" subStdoutVisibility ++
            maybePair "extra_data"        subExtraData ++
            maybePair "tests"             subTests ++
            maybePair "leaderboard"       subLeaderboard

defaultResults :: SubmissionResults
defaultResults = SubmissionResults
    { subScore            = Nothing
    , subExecutionTime    = Nothing
    , subOutput           = Nothing
    , subOutputFormat     = Nothing
    , subTestOutputFormat = Nothing
    , subTestNameFormat   = Nothing
    , subVisibility       = Nothing
    , subStdoutVisibility = Nothing
    , subExtraData        = Nothing
    , subTests            = Nothing
    , subLeaderboard      = Nothing
    }