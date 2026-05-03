module ZeitLingq.Core.Browse
  ( hideIgnoredSummaries
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import ZeitLingq.Domain.Types (ArticleSummary(..))

hideIgnoredSummaries :: Set Text -> [ArticleSummary] -> [ArticleSummary]
hideIgnoredSummaries ignoredUrls =
  filter (not . (`Set.member` ignoredUrls) . summaryUrl)
