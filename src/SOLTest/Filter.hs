-- | Filtering test cases by include and exclude criteria.
--
-- The filtering algorithm is a two-phase set operation:
--
-- 1. __Include__: if no include criteria are given, all tests are included;
--    otherwise only tests matching at least one include criterion are kept.
--
-- 2. __Exclude__: tests matching any exclude criterion are removed from the
--    included set.
module SOLTest.Filter
  ( filterTests,
    matchesCriterion,
    matchesAny,
    trimFilterId,
  )
where

import Data.Char (isSpace)
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Apply a 'FilterSpec' to a list of test definitions.
--
-- Returns a pair @(selected, filteredOut)@ where:
--
-- * @selected@ are the tests that passed both include and exclude checks.
-- * @filteredOut@ are the tests that were removed by filtering.
--
-- The union of @selected@ and @filteredOut@ always equals the input list.
--
-- FLP: Implement this function using @matchesAny@ and @matchesCriterion@.
filterTests ::
  FilterSpec ->
  [TestCaseDefinition] ->
  ([TestCaseDefinition], [TestCaseDefinition])
-- filterTests spec tests = foldr filterOne ([],[]) tests
filterTests spec = foldr filterOne ([],[]) -- go through every test with foldr and divide the test into two sets
  where
    filterOne testDef acc =
      case fsIncludes spec of
        [] -> excludeInsert testDef acc   -- No include-criteria means include all tests (exclude from all tests)
        _  -> includeInsert testDef acc

    -- check whether test meets any criteria
    includeInsert testDef (sel_acc, fil_acc)
      | matchesAny (fsIncludes spec) testDef = excludeInsert testDef (sel_acc, fil_acc) -- it does -> further check
      | otherwise                            = (sel_acc, testDef : fil_acc)             -- it doesn't -> exclude

    -- check whether test meets any excluding criteria
    excludeInsert testDef (sel_acc, fil_acc)
      | matchesAny (fsExcludes spec) testDef = (sel_acc, testDef : fil_acc)             -- it does -> exclude
      | otherwise                            = (testDef : sel_acc, fil_acc)             -- it doesn't -> include


-- | Check whether a test matches at least one criterion in the list.
matchesAny :: [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny criteria test =
  any (matchesCriterion test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- FLP: Implement this function.
matchesCriterion :: TestCaseDefinition -> FilterCriterion -> Bool
matchesCriterion test (ByAny criterion) = crit == tcdName test || crit == tcdCategory test || crit `elem` tcdTags test
    where crit = trimFilterId criterion
matchesCriterion test (ByCategory criterion) = trimFilterId criterion == tcdCategory test
matchesCriterion test (ByTag criterion) = trimFilterId criterion `elem` tcdTags test

-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace
