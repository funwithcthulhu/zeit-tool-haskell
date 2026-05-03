module ZeitLingq.Core.Browse
  ( hideIgnoredSummaries
  , markIgnoredSummaries
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import ZeitLingq.Domain.Types (ArticleSummary(..))

hideIgnoredSummaries :: Set Text -> [ArticleSummary] -> [ArticleSummary]
hideIgnoredSummaries ignoredUrls =
  filter (not . (`Set.member` ignoredUrls) . summaryUrl)

markIgnoredSummaries :: Set Text -> [ArticleSummary] -> [ArticleSummary]
markIgnoredSummaries ignoredUrls =
  map markOne
  where
    markOne summary =
      summary {summaryIgnored = summaryIgnored summary || Set.member (summaryUrl summary) ignoredUrls}
