-- | Building the final test report and computing statistics.
--
-- This module assembles a 'TestReport' from the results of test execution,
-- computes aggregate statistics, and builds the per-category success-rate
-- histogram.
module SOLTest.Report
  ( buildReport,
    groupByCategory,
    computeStats,
    computeHistogram,
    rateToBin,
  )
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import SOLTest.Types
-- import SOLTest.ReportSpec (reportProps) -- what

-- ---------------------------------------------------------------------------
-- Top-level report assembly
-- ---------------------------------------------------------------------------

-- | Assemble the complete 'TestReport'.
--
-- Parameters:
--
-- * @discovered@ – all 'TestCaseDefinition' values that were successfully parsed.
-- * @unexecuted@ – tests that were not executed for any reason (filtered, malformed, etc.).
-- * @executionResults@ – 'Nothing' in dry-run mode; otherwise the map of test
--   results keyed by test name.
-- * @selected@ – the tests that were selected for execution (used for stats).
-- * @foundCount@ – total number of @.test@ files discovered on disk.
buildReport ::
  [TestCaseDefinition] ->
  Map String UnexecutedReason ->
  Maybe (Map String TestCaseReport) ->
  [TestCaseDefinition] ->
  Int ->
  TestReport
buildReport discovered unexecuted mResults selected foundCount =
  let mCategoryResults = fmap (groupByCategory selected) mResults
      stats = computeStats foundCount (length discovered) (length selected) mCategoryResults
   in TestReport
        { trDiscoveredTestCases = discovered,
          trUnexecuted = unexecuted,
          trResults = mCategoryResults,
          trStats = stats
        }

-- ---------------------------------------------------------------------------
-- Grouping and category reports
-- ---------------------------------------------------------------------------

-- | Group a flat map of test results into a map of 'CategoryReport' values,
-- one per category.
--
-- The @definitions@ list is used to look up each test's category and points.
--
-- FLP: Implement this function. The following functions may (or may not) come in handy:
--      @Map.fromList@, @Map.foldlWithKey'@, @Map.empty@, @Map.lookup@, @Map.insertWith@,
--      @Map.map@, @Map.fromList@
groupByCategory ::
  [TestCaseDefinition] ->
  Map String TestCaseReport ->
  Map String CategoryReport
groupByCategory definitions results = 
  --                   key: tcdName, value TestCaseDefinition | go through the list and for every TCD do
  let defMap = Map.fromList [(tcdName definition, definition)| definition <- definitions]

      -- acc function 
      accFunc acc resName resReport = 
        -- looks up the definition of current test result
        case Map.lookup resName defMap of
          Nothing -> acc
          Just testDef ->
            let
              -- extracts cathegory and creates CategoryReport just for this one test result
              -- cathegory is a key in the resulting map (acc)
              cat = tcdCategory testDef
              newCatReport = makeCatReport testDef resReport
            in
              -- inserts it into acc map
              -- if the report for this category isn't there yet, simply inserts
              -- if it is there, uses insertCat function to add it
              Map.insertWith insertCat cat newCatReport acc
  in
    -- go through all the results
    Map.foldlWithKey' accFunc Map.empty results 

-- creates CategoryReport for one result
makeCatReport :: TestCaseDefinition -> TestCaseReport -> CategoryReport
makeCatReport def testReport =
  let 
    testname = tcdName def                                                  -- test filename
    defPoints = tcdPoints def                                               -- max points
    passedPoints = if tcrResult testReport == Passed then defPoints else 0  -- recieved points
  in
    -- create report
    CategoryReport
      { crTotalPoints = defPoints,
        crPassedPoints = passedPoints,
        crTestResults = Map.singleton testname testReport
      }

-- joins two CathegoryReports of the same cathegory
insertCat :: CategoryReport -> CategoryReport -> CategoryReport
insertCat newCatReport oldCatReport =
  let
    -- simply add the points and joins the two TestCaseReport maps
    total = crTotalPoints oldCatReport + crTotalPoints newCatReport
    totalPassed = crPassedPoints oldCatReport + crPassedPoints newCatReport
    results = Map.union (crTestResults oldCatReport) (crTestResults newCatReport)
  in
  CategoryReport
    { crTotalPoints = total,
      crPassedPoints = totalPassed,
      crTestResults = results
    }

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Compute the 'TestStats' from available information.
--
-- FLP: Implement this function. You'll use @computeHistogram@ here.
computeStats ::
  -- | Total @.test@ files found on disk.
  Int ->
  -- | Number of successfully parsed tests.
  Int ->
  -- | Number of tests selected after filtering.
  Int ->
  -- | Category reports (Nothing in dry-run mode).
  Maybe (Map String CategoryReport) ->
  TestStats
computeStats foundCount loadedCount selectedCount mCategoryResults = 
  let
    -- unpack Maybe results and compute stats
    passedTests = maybe 0 computePassed mCategoryResults
    hist = maybe Map.empty computeHistogram mCategoryResults
    -- alternative version
    -- -------------------
    -- passedTests = 
    --   case mCategoryResults of
    --     Nothing -> 0
    --     Just categoryResults -> computePassed categoryResults
    -- hist =
    --   case mCategoryResults of
    --     Nothing -> Map.empty
    --     Just categoryResults -> computeHistogram categoryResults
  in
    TestStats
      { tsFoundTestFiles = foundCount,
        tsLoadedTests = loadedCount,
        tsSelectedTests = selectedCount,
        tsPassedTests = passedTests,
        tsHistogram = hist
      }

-- Go througgh all the cat reports, look through each test report and 
-- count the successful tests
computePassed :: Map String CategoryReport -> Int
computePassed catReports = 
  let
    -- map -> list
    categoriesList = Map.elems catReports

    -- sucessful tests in one cat (cat report)
    countPassedInCat :: CategoryReport -> Int
    countPassedInCat cat =
      let 
        -- get list of TestCaseReports
        allTestsInCat = Map.elems (crTestResults cat)
        -- filter out all unsuccessful ones
        passedTests = filter (\testReport -> tcrResult testReport == Passed) allTestsInCat
      in 
        -- count them
        length passedTests
  in
    -- sum successes for each cat
    sum (map countPassedInCat categoriesList)


-- ---------------------------------------------------------------------------
-- Histogram
-- ---------------------------------------------------------------------------

-- | Compute the success-rate histogram from the category reports.
--
-- For each category, the relative pass rate is:
--
-- @rate = passed_test_count \/ total_test_count@
--
-- The rate is mapped to a bin key (@\"0.0\"@ through @\"0.9\"@) and the count
-- of categories in each bin is accumulated. All ten bins are always present in
-- the result, even if their count is 0.
--
-- FLP: Implement this function.
computeHistogram :: Map String CategoryReport -> Map String Int
computeHistogram categories = 
  let
    -- map to [CategoryReport]
    categoriesList = Map.elems categories
    -- empty hist used as acc in fold
    emptyHist = Map.fromList 
      [("0.0", 0), ("0.1", 0), ("0.2", 0), ("0.3", 0), ("0.4", 0),
      ("0.5", 0), ("0.6", 0), ("0.7", 0), ("0.8", 0), ("0.9", 0)]
  in 
    -- go through every cathegory in the list, compute and add 1 to resulting bin
    foldl' addToHist emptyHist categoriesList


addToHist :: Map String Int -> CategoryReport -> Map String Int
addToHist hist cat =
  let
    allTestsInCat = Map.elems (crTestResults cat)
    passedTests = filter (\testReport -> tcrResult testReport == Passed) allTestsInCat
    testCount = length allTestsInCat
    passedTestCount = length passedTests
    rate = fromIntegral passedTestCount / fromIntegral testCount 
  in
    Map.insertWith (+) (rateToBin rate) 1 hist


-- | Map a pass rate in @[0, 1]@ to a histogram bin key.
--
-- Bins are defined as @[0.0, 0.1)@, @[0.1, 0.2)@, ..., @[0.9, 1.0]@.
-- A rate of exactly @1.0@ maps to the @\"0.9\"@ bin.
rateToBin :: Double -> String
rateToBin rate =
  let binIndex = min 9 (floor (rate * 10) :: Int)
      -- Format as "0.N" for bin index N
      whole = binIndex `div` 10
      frac = binIndex `mod` 10
   in show whole ++ "." ++ show frac
