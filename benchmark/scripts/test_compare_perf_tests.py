#!/usr/bin/python
# -*- coding: utf-8 -*-

# ===--- test_compare_perf_tests.py --------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https://swift.org/LICENSE.txt for license information
#  See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===//

import os
import shutil
import sys
import tempfile
import unittest

from compare_perf_tests import LogParser
from compare_perf_tests import PerformanceTestResult
from compare_perf_tests import PerformanceTestSamples
from compare_perf_tests import ReportFormatter
from compare_perf_tests import ResultComparison
from compare_perf_tests import Sample
from compare_perf_tests import TestComparator
from compare_perf_tests import main
from compare_perf_tests import parse_args

from test_utils import captured_output


class TestSample(unittest.TestCase):
    def test_has_named_fields(self):
        s = Sample(1, 2, 3)
        self.assertEquals(s.i, 1)
        self.assertEquals(s.num_iters, 2)
        self.assertEquals(s.runtime, 3)

    def test_is_iterable(self):
        s = Sample(1, 2, 3)
        self.assertEquals(s[0], 1)
        self.assertEquals(s[1], 2)
        self.assertEquals(s[2], 3)


class TestPerformanceTestSamples(unittest.TestCase):
    def setUp(self):
        self.samples = PerformanceTestSamples('B1')
        self.samples.add(Sample(7, 42, 1000))

    def test_has_name(self):
        self.assertEquals(self.samples.name, 'B1')

    def test_stores_samples(self):
        self.assertEquals(self.samples.count, 1)
        s = self.samples.samples[0]
        self.assertTrue(isinstance(s, Sample))
        self.assertEquals(s.i, 7)
        self.assertEquals(s.num_iters, 42)
        self.assertEquals(s.runtime, 1000)

    def test_quantile(self):
        self.assertEquals(self.samples.quantile(1), 1000)
        self.assertEquals(self.samples.quantile(0), 1000)
        self.samples.add(Sample(2, 1, 1100))
        self.assertEquals(self.samples.quantile(0), 1000)
        self.assertEquals(self.samples.quantile(1), 1100)
        self.samples.add(Sample(3, 1, 1050))
        self.assertEquals(self.samples.quantile(0), 1000)
        self.assertEquals(self.samples.quantile(.5), 1050)
        self.assertEquals(self.samples.quantile(1), 1100)

    def assertEqualFiveNumberSummary(self, ss, expected_fns):
        e_min, e_q1, e_median, e_q3, e_max = expected_fns
        self.assertEquals(ss.min, e_min)
        self.assertEquals(ss.q1, e_q1)
        self.assertEquals(ss.median, e_median)
        self.assertEquals(ss.q3, e_q3)
        self.assertEquals(ss.max, e_max)

    def test_computes_five_number_summary(self):
        self.assertEqualFiveNumberSummary(
            self.samples, (1000, 1000, 1000, 1000, 1000))
        self.samples.add(Sample(2, 1, 1100))
        self.assertEqualFiveNumberSummary(
            self.samples, (1000, 1000, 1000, 1100, 1100))
        self.samples.add(Sample(3, 1, 1050))
        self.assertEqualFiveNumberSummary(
            self.samples, (1000, 1000, 1050, 1100, 1100))
        self.samples.add(Sample(4, 1, 1025))
        self.assertEqualFiveNumberSummary(
            self.samples, (1000, 1000, 1025, 1050, 1100))
        self.samples.add(Sample(5, 1, 1075))
        self.assertEqualFiveNumberSummary(
            self.samples, (1000, 1025, 1050, 1075, 1100))

    def test_computes_inter_quartile_range(self):
        self.assertEquals(self.samples.iqr, 0)
        self.samples.add(Sample(2, 1, 1025))
        self.samples.add(Sample(3, 1, 1050))
        self.samples.add(Sample(4, 1, 1075))
        self.samples.add(Sample(5, 1, 1100))
        self.assertEquals(self.samples.iqr, 50)

    def assertEqualStats(self, stats, expected_stats):
        for actual, expected in zip(stats, expected_stats):
            self.assertAlmostEquals(actual, expected, places=2)

    def test_computes_mean_sd_cv(self):
        ss = self.samples
        self.assertEqualStats(
            (ss.mean, ss.sd, ss.cv), (1000.0, 0.0, 0.0))
        self.samples.add(Sample(2, 1, 1100))
        self.assertEqualStats(
            (ss.mean, ss.sd, ss.cv), (1050.0, 70.71, 6.7 / 100))

    def test_computes_range_spread(self):
        ss = self.samples
        self.assertEqualStats(
            (ss.range, ss.spread), (0, 0))
        self.samples.add(Sample(2, 1, 1100))
        self.assertEqualStats(
            (ss.range, ss.spread), (100, 10.0 / 100))

    def test_init_with_samples(self):
        self.samples = PerformanceTestSamples(
            'B2', [Sample(0, 1, 1000), Sample(1, 1, 1100)])
        self.assertEquals(self.samples.count, 2)
        self.assertEqualStats(
            (self.samples.mean, self.samples.sd,
             self.samples.range, self.samples.spread),
            (1050.0, 70.71, 100, 9.52 / 100))

    def test_can_handle_zero_runtime(self):
        # guard against dividing by 0
        self.samples = PerformanceTestSamples('Zero')
        self.samples.add(Sample(0, 1, 0))
        self.assertEqualStats(
            (self.samples.mean, self.samples.sd, self.samples.cv,
             self.samples.range, self.samples.spread),
            (0, 0, 0.0, 0, 0.0))

    def test_excludes_outliers(self):
        ss = [Sample(*map(int, s.split())) for s in
              '0 1 1000, 1 1 1025, 2 1 1050, 3 1 1075, 4 1 1100, '
              '5 1 1000, 6 1 1025, 7 1 1050, 8 1 1075, 9 1 1100, '
              '10 1 1050, 11 1 949, 12 1 1151'.split(',')]
        self.samples = PerformanceTestSamples('Outliers', ss)
        self.assertEquals(self.samples.count, 13)
        self.assertEqualStats(
            (self.samples.mean, self.samples.sd), (1050, 52.36))

        self.samples.exclude_outliers()

        self.assertEquals(self.samples.count, 11)
        self.assertEquals(self.samples.outliers, ss[11:])
        self.assertEqualFiveNumberSummary(
            self.samples, (1000, 1025, 1050, 1075, 1100))
        self.assertEqualStats(
            (self.samples.mean, self.samples.sd), (1050, 35.36))

    def test_excludes_outliers_zero_IQR(self):
        self.samples = PerformanceTestSamples('Tight')
        self.samples.add(Sample(0, 2, 23))
        self.samples.add(Sample(1, 2, 18))
        self.samples.add(Sample(2, 2, 18))
        self.samples.add(Sample(3, 2, 18))
        self.assertEquals(self.samples.iqr, 0)

        self.samples.exclude_outliers()

        self.assertEquals(self.samples.count, 3)
        self.assertEqualStats(
            (self.samples.min, self.samples.max), (18, 18))

    def test_excludes_outliers_top_only(self):
        ss = [Sample(*map(int, s.split())) for s in
              '0 1 1, 1 1 2, 2 1 2, 3 1 2, 4 1 3'.split(',')]
        self.samples = PerformanceTestSamples('Top', ss)
        self.assertEqualFiveNumberSummary(self.samples, (1, 2, 2, 2, 3))
        self.assertEquals(self.samples.iqr, 0)

        self.samples.exclude_outliers(top_only=True)

        self.assertEquals(self.samples.count, 4)
        self.assertEqualStats((self.samples.min, self.samples.max), (1, 2))


class TestPerformanceTestResult(unittest.TestCase):
    def test_init(self):
        log_line = '1,AngryPhonebook,20,10664,12933,11035,576,10884'
        r = PerformanceTestResult(log_line.split(','))
        self.assertEquals(r.test_num, '1')
        self.assertEquals(r.name, 'AngryPhonebook')
        self.assertEquals(
            (r.num_samples, r.min, r.max, r.mean, r.sd, r.median),
            (20, 10664, 12933, 11035, 576, 10884))
        self.assertEquals(r.samples, None)

        log_line = '1,AngryPhonebook,1,12045,12045,12045,0,12045,10510336'
        r = PerformanceTestResult(log_line.split(','))
        self.assertEquals(r.max_rss, 10510336)

    def test_init_quantiles(self):
        # #,TEST,SAMPLES,MIN(μs),MEDIAN(μs),MAX(μs)
        log = '1,Ackermann,3,54383,54512,54601'
        r = PerformanceTestResult(log.split(','), quantiles=True)
        self.assertEquals(r.test_num, '1')
        self.assertEquals(r.name, 'Ackermann')
        self.assertEquals((r.num_samples, r.min, r.median, r.max),
                          (3, 54383, 54512, 54601))
        self.assertAlmostEquals(r.mean, 54498.67, places=2)
        self.assertAlmostEquals(r.sd, 109.61, places=2)
        self.assertEquals(r.samples.count, 3)
        self.assertEquals(r.samples.num_samples, 3)
        self.assertEquals([s.runtime for s in r.samples.all_samples],
                          [54383, 54512, 54601])

        # #,TEST,SAMPLES,MIN(μs),MEDIAN(μs),MAX(μs),MAX_RSS(B)
        log = '1,Ackermann,3,54529,54760,55807,266240'
        r = PerformanceTestResult(log.split(','), quantiles=True, memory=True)
        self.assertEquals((r.samples.count, r.max_rss), (3, 266240))
        # #,TEST,SAMPLES,MIN(μs),Q1(μs),Q2(μs),Q3(μs),MAX(μs)
        log = '1,Ackermann,5,54570,54593,54644,57212,58304'
        r = PerformanceTestResult(log.split(','), quantiles=True, memory=False)
        self.assertEquals((r.num_samples, r.min, r.median, r.max),
                          (5, 54570, 54644, 58304))
        self.assertEquals((r.samples.q1, r.samples.q3), (54593, 57212))
        self.assertEquals(r.samples.count, 5)
        # #,TEST,SAMPLES,MIN(μs),Q1(μs),Q2(μs),Q3(μs),MAX(μs),MAX_RSS(B)
        log = '1,Ackermann,5,54686,54731,54774,55030,63466,270336'
        r = PerformanceTestResult(log.split(','), quantiles=True, memory=True)
        self.assertEquals(r.samples.num_samples, 5)
        self.assertEquals(r.samples.count, 4)  # outlier was excluded
        self.assertEquals(r.max_rss, 270336)

    def test_init_delta_quantiles(self):
        # #,TEST,SAMPLES,MIN(μs),𝚫MEDIAN,𝚫MAX
        # 2-quantile from 2 samples in repeated min, when delta encoded,
        # the difference is 0, which is ommited -- only separator remains
        log = '202,DropWhileArray,2,265,,22'
        r = PerformanceTestResult(log.split(','), quantiles=True, delta=True)
        self.assertEquals((r.num_samples, r.min, r.median, r.max),
                          (2, 265, 265, 287))
        self.assertEquals(r.samples.count, 2)
        self.assertEquals(r.samples.num_samples, 2)

    def test_init_oversampled_quantiles(self):
        """When num_samples is < quantile + 1, some of the measurements are
        repeated in the report summary. Samples should contain only true
        values, discarding the repetated artifacts from quantile estimation.

        The test string is slightly massaged output of the following R script:
        subsample <- function(x, q) {
          quantile(1:x, probs=((0:(q-1))/(q-1)), type=1)}
        tbl <- function(s) t(sapply(1:s, function(x) {
          qs <- subsample(x, s); c(qs[1], diff(qs)) }))
        sapply(c(3, 5, 11, 21), tbl)
        """
        def validatePTR(deq):  # construct from delta encoded quantiles string
            deq = deq.split(',')
            num_samples = deq.count('1')
            r = PerformanceTestResult(['0', 'B', str(num_samples)] + deq,
                                      quantiles=True, delta=True)
            self.assertEquals(r.samples.num_samples, num_samples)
            self.assertEquals([s.runtime for s in r.samples.all_samples],
                              range(1, num_samples + 1))

        delta_encoded_quantiles = """
1,,
1,,1
1,,,,
1,,,1,
1,,1,1,
1,,1,1,1
1,,,,,,,,,,
1,,,,,,1,,,,
1,,,,1,,,1,,,
1,,,1,,,1,,1,,
1,,,1,,1,,1,,1,
1,,1,,1,,1,1,,1,
1,,1,1,,1,1,,1,1,
1,,1,1,1,,1,1,1,1,
1,,1,1,1,1,1,1,1,1,
1,,1,1,1,1,1,1,1,1,1
1,,,,,,,,,,,,,,,,,,,,
1,,,,,,,,,,,1,,,,,,,,,
1,,,,,,,1,,,,,,,1,,,,,,
1,,,,,,1,,,,,1,,,,,1,,,,
1,,,,,1,,,,1,,,,1,,,,1,,,
1,,,,1,,,1,,,,1,,,1,,,1,,,
1,,,1,,,1,,,1,,,1,,,1,,,1,,
1,,,1,,,1,,1,,,1,,1,,,1,,1,,
1,,,1,,1,,1,,1,,,1,,1,,1,,1,,
1,,,1,,1,,1,,1,,1,,1,,1,,1,,1,
1,,1,,1,,1,,1,,1,1,,1,,1,,1,,1,
1,,1,,1,,1,1,,1,,1,1,,1,,1,1,,1,
1,,1,,1,1,,1,1,,1,1,,1,1,,1,1,,1,
1,,1,1,,1,1,,1,1,,1,1,1,,1,1,,1,1,
1,,1,1,,1,1,1,,1,1,1,,1,1,1,,1,1,1,
1,,1,1,1,,1,1,1,1,,1,1,1,1,,1,1,1,1,
1,,1,1,1,1,1,,1,1,1,1,1,1,,1,1,1,1,1,
1,,1,1,1,1,1,1,1,1,,1,1,1,1,1,1,1,1,1,
1,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1"""
        map(validatePTR, delta_encoded_quantiles.split('\n')[1:])

    def test_repr(self):
        log_line = '1,AngryPhonebook,20,10664,12933,11035,576,10884'
        r = PerformanceTestResult(log_line.split(','))
        self.assertEquals(
            str(r),
            '<PerformanceTestResult name:\'AngryPhonebook\' samples:20 '
            'min:10664 max:12933 mean:11035 sd:576 median:10884>'
        )

    def test_merge(self):
        tests = """
1,AngryPhonebook,1,12045,12045,12045,0,12045
1,AngryPhonebook,1,12325,12325,12325,0,12325,10510336
1,AngryPhonebook,1,11616,11616,11616,0,11616,10502144
1,AngryPhonebook,1,12270,12270,12270,0,12270,10498048""".split('\n')[1:]
        results = map(PerformanceTestResult,
                      [line.split(',') for line in tests])
        results[2].setup = 9
        results[3].setup = 7

        def as_tuple(r):
            return (r.num_samples, r.min, r.max, round(r.mean, 2),
                    r.sd, r.median, r.max_rss, r.setup)

        r = results[0]
        self.assertEquals(as_tuple(r),
                          (1, 12045, 12045, 12045, 0, 12045, None, None))
        r.merge(results[1])
        self.assertEquals(as_tuple(r),  # drops SD and median, +max_rss
                          (2, 12045, 12325, 12185, None, None, 10510336, None))
        r.merge(results[2])
        self.assertEquals(as_tuple(r),  # picks smaller of the MAX_RSS, +setup
                          (3, 11616, 12325, 11995.33, None, None, 10502144, 9))
        r.merge(results[3])
        self.assertEquals(as_tuple(r),  # picks smaller of the setup values
                          (4, 11616, 12325, 12064, None, None, 10498048, 7))


class TestResultComparison(unittest.TestCase):
    def setUp(self):
        self.r0 = PerformanceTestResult(
            '101,GlobalClass,20,0,0,0,0,0,10185728'.split(','))
        self.r01 = PerformanceTestResult(
            '101,GlobalClass,20,20,20,20,0,0,10185728'.split(','))
        self.r1 = PerformanceTestResult(
            '1,AngryPhonebook,1,12325,12325,12325,0,12325,10510336'.split(','))
        self.r2 = PerformanceTestResult(
            '1,AngryPhonebook,1,11616,11616,11616,0,11616,10502144'.split(','))

    def test_init(self):
        rc = ResultComparison(self.r1, self.r2)
        self.assertEquals(rc.name, 'AngryPhonebook')
        self.assertAlmostEquals(rc.ratio, 12325.0 / 11616.0)
        self.assertAlmostEquals(rc.delta, (((11616.0 / 12325.0) - 1) * 100),
                                places=3)
        # handle test results that sometimes change to zero, when compiler
        # optimizes out the body of the incorrectly written test
        rc = ResultComparison(self.r0, self.r0)
        self.assertEquals(rc.name, 'GlobalClass')
        self.assertAlmostEquals(rc.ratio, 1)
        self.assertAlmostEquals(rc.delta, 0, places=3)
        rc = ResultComparison(self.r0, self.r01)
        self.assertAlmostEquals(rc.ratio, 0, places=3)
        self.assertAlmostEquals(rc.delta, 2000000, places=3)
        rc = ResultComparison(self.r01, self.r0)
        self.assertAlmostEquals(rc.ratio, 20001)
        self.assertAlmostEquals(rc.delta, -99.995, places=3)
        # disallow comparison of different test results
        self.assertRaises(
            AssertionError,
            ResultComparison, self.r0, self.r1
        )

    def test_values_is_dubious(self):
        self.assertFalse(ResultComparison(self.r1, self.r2).is_dubious)
        self.r2.max = self.r1.min + 1
        # new.min < old.min < new.max
        self.assertTrue(ResultComparison(self.r1, self.r2).is_dubious)
        # other way around: old.min < new.min < old.max
        self.assertTrue(ResultComparison(self.r2, self.r1).is_dubious)


class FileSystemIntegration(unittest.TestCase):
    def setUp(self):
        # Create a temporary directory
        self.test_dir = tempfile.mkdtemp()

    def tearDown(self):
        # Remove the directory after the test
        shutil.rmtree(self.test_dir)

    def write_temp_file(self, file_name, data):
        temp_file_name = os.path.join(self.test_dir, file_name)
        with open(temp_file_name, 'w') as f:
            f.write(data)
        return temp_file_name


class OldAndNewLog(unittest.TestCase):
    old_log_content = """1,AngryPhonebook,20,10458,12714,11000,0,11000,10204365
2,AnyHashableWithAClass,20,247027,319065,259056,0,259056,10250445
3,Array2D,20,335831,400221,346622,0,346622,28297216
4,ArrayAppend,20,23641,29000,24990,0,24990,11149926
34,BitCount,20,3,4,4,0,4,10192896
35,ByteSwap,20,4,6,4,0,4,10185933"""

    new_log_content = """265,TwoSum,20,5006,5679,5111,0,5111
35,ByteSwap,20,0,0,0,0,0
34,BitCount,20,9,9,9,0,9
4,ArrayAppend,20,20000,29000,24990,0,24990
3,Array2D,20,335831,400221,346622,0,346622
1,AngryPhonebook,20,10458,12714,11000,0,11000"""

    old_results = dict([(r.name, r)
                        for r in
                        map(PerformanceTestResult,
                            [line.split(',')
                             for line in
                             old_log_content.splitlines()])])

    new_results = dict([(r.name, r)
                        for r in
                        map(PerformanceTestResult,
                            [line.split(',')
                             for line in
                             new_log_content.splitlines()])])

    def assert_report_contains(self, texts, report):
        assert not isinstance(texts, str)
        for text in texts:
            self.assertIn(text, report)


class TestLogParser(unittest.TestCase):
    def test_parse_results_csv(self):
        """Ignores uknown lines, extracts data from supported formats."""
        log = """#,TEST,SAMPLES,MIN(us),MAX(us),MEAN(us),SD(us),MEDIAN(us)
34,BitCount,20,3,4,4,0,4

Total performance tests executed: 1
"""
        parser = LogParser()
        results = parser.parse_results(log.splitlines())
        self.assertTrue(isinstance(results[0], PerformanceTestResult))
        self.assertEquals(results[0].name, 'BitCount')

    def test_parse_results_tab_delimited(self):
        log = '34\tBitCount\t20\t3\t4\t4\t0\t4'
        parser = LogParser()
        results = parser.parse_results(log.splitlines())
        self.assertTrue(isinstance(results[0], PerformanceTestResult))
        self.assertEquals(results[0].name, 'BitCount')

    def test_parse_results_formatted_text(self):
        """Parse format that Benchmark_Driver prints to console"""
        log = ("""
  # TEST      SAMPLES MIN(μs) MAX(μs) MEAN(μs) SD(μs) MEDIAN(μs) MAX_RSS(B)
  3 Array2D        20    2060    2188     2099      0       2099   20915200

Total performance tests executed: 1
""")
        parser = LogParser()
        results = parser.parse_results(log.splitlines()[1:])  # without 1st \n
        self.assertTrue(isinstance(results[0], PerformanceTestResult))
        r = results[0]
        self.assertEquals(r.name, 'Array2D')
        self.assertEquals(r.max_rss, 20915200)

    def test_parse_quantiles(self):
        """Gathers samples from reported quantiles. Handles optional memory."""
        r = LogParser.results_from_string(
            """#,TEST,SAMPLES,MIN(μs),MEDIAN(μs),MAX(μs)
1,Ackermann,3,54383,54512,54601""")['Ackermann']
        self.assertEquals([s.runtime for s in r.samples.all_samples],
                          [54383, 54512, 54601])
        r = LogParser.results_from_string(
            """#,TEST,SAMPLES,MIN(μs),MEDIAN(μs),MAX(μs),MAX_RSS(B)
1,Ackermann,3,54529,54760,55807,266240""")['Ackermann']
        self.assertEquals([s.runtime for s in r.samples.all_samples],
                          [54529, 54760, 55807])
        self.assertEquals(r.max_rss, 266240)

    def test_parse_delta_quantiles(self):
        r = LogParser.results_from_string(  # 2-quantile aka. median
            '#,TEST,SAMPLES,MIN(μs),𝚫MEDIAN,𝚫MAX\n0,B,1,101,,')['B']
        self.assertEquals(
            (r.num_samples, r.min, r.median, r.max, r.samples.count),
            (1, 101, 101, 101, 1))
        r = LogParser.results_from_string(
            '#,TEST,SAMPLES,MIN(μs),𝚫MEDIAN,𝚫MAX\n0,B,2,101,,1')['B']
        self.assertEquals(
            (r.num_samples, r.min, r.median, r.max, r.samples.count),
            (2, 101, 101, 102, 2))
        r = LogParser.results_from_string(  # 20-quantiles aka. ventiles
            '#,TEST,SAMPLES,MIN(μs),𝚫V1,𝚫V2,𝚫V3,𝚫V4,𝚫V5,𝚫V6,𝚫V7,𝚫V8,' +
            '𝚫V9,𝚫VA,𝚫VB,𝚫VC,𝚫VD,𝚫VE,𝚫VF,𝚫VG,𝚫VH,𝚫VI,𝚫VJ,𝚫MAX\n' +
            '202,DropWhileArray,200,214,,,,,,,,,,,,1,,,,,,2,16,464'
        )['DropWhileArray']
        self.assertEquals(
            (r.num_samples, r.min, r.max, r.samples.count),
            # last 3 ventiles were outliers and were excluded from the sample
            (200, 214, 215, 18))

    def test_parse_results_verbose(self):
        """Parse multiple performance test results with 2 sample formats:
        single line for N = 1; two lines for N > 1.
        """
        verbose_log = """--- DATA ---
#,TEST,SAMPLES,MIN(us),MAX(us),MEAN(us),SD(us),MEDIAN(us)
Running AngryPhonebook for 3 samples.
    Measuring with scale 78.
    Sample 0,11812
    Measuring with scale 90.
    Sample 1,13898
    Sample 2,11467
1,AngryPhonebook,3,11467,13898,12392,1315,11812
Running Array2D for 3 samples.
    SetUp 14444
    Sample 0,369900
    Yielding after ~369918 μs
    Sample 1,381039
    Yielding after ~381039 μs
    Sample 2,371043
3,Array2D,3,369900,381039,373994,6127,371043

Totals,2"""
        parser = LogParser()
        results = parser.parse_results(verbose_log.split('\n'))

        r = results[0]
        self.assertEquals(
            (r.name, r.min, r.max, int(r.mean), int(r.sd), r.median),
            ('AngryPhonebook', 11467, 13898, 12392, 1315, 11812)
        )
        self.assertEquals(r.num_samples, r.samples.num_samples)
        self.assertEquals(results[0].samples.all_samples,
                          [(0, 78, 11812), (1, 90, 13898), (2, 90, 11467)])
        self.assertEquals(r.yields, None)

        r = results[1]
        self.assertEquals(
            (r.name, r.min, r.max, int(r.mean), int(r.sd), r.median),
            ('Array2D', 369900, 381039, 373994, 6127, 371043)
        )
        self.assertEquals(r.setup, 14444)
        self.assertEquals(r.num_samples, r.samples.num_samples)
        self.assertEquals(results[1].samples.all_samples,
                          [(0, 1, 369900), (1, 1, 381039), (2, 1, 371043)])
        yielded = r.yields[0]
        self.assertEquals(yielded.before_sample, 1)
        self.assertEquals(yielded.after, 369918)
        self.assertEquals(r.yields, [(1, 369918), (2, 381039)])

    def test_parse_environment_verbose(self):
        """Parse stats about environment in verbose mode."""
        verbose_log = """    MAX_RSS 8937472 - 8904704 = 32768 (8 pages)
    ICS 1338 - 229 = 1109
    VCS 2 - 1 = 1
2,AngryPhonebook,3,11269,11884,11657,338,11820
"""
        parser = LogParser()
        results = parser.parse_results(verbose_log.split('\n'))

        r = results[0]
        self.assertEquals(r.max_rss, 32768)
        self.assertEquals(r.mem_pages, 8)
        self.assertEquals(r.voluntary_cs, 1)
        self.assertEquals(r.involuntary_cs, 1109)

    def test_results_from_merge(self):
        """Parsing concatenated log merges same PerformanceTestResults"""
        concatenated_logs = """4,ArrayAppend,20,23641,29000,24990,0,24990
4,ArrayAppend,1,20000,20000,20000,0,20000"""
        results = LogParser.results_from_string(concatenated_logs)
        self.assertEquals(results.keys(), ['ArrayAppend'])
        result = results['ArrayAppend']
        self.assertTrue(isinstance(result, PerformanceTestResult))
        self.assertEquals(result.min, 20000)
        self.assertEquals(result.max, 29000)

    def test_results_from_merge_verbose(self):
        """Parsing verbose log  merges all PerformanceTestSamples.
        ...this should technically be on TestPerformanceTestResult, but it's
        easier to write here. ¯\_(ツ)_/¯"""
        concatenated_logs = """
    Sample 0,355883
    Sample 1,358817
    Sample 2,353552
    Sample 3,350815
3,Array2D,4,350815,358817,354766,3403,355883
    Sample 0,363094
    Sample 1,369169
    Sample 2,376131
    Sample 3,364245
3,Array2D,4,363094,376131,368159,5931,369169"""
        results = LogParser.results_from_string(concatenated_logs)
        self.assertEquals(results.keys(), ['Array2D'])
        result = results['Array2D']
        self.assertTrue(isinstance(result, PerformanceTestResult))
        self.assertEquals(result.min, 350815)
        self.assertEquals(result.max, 376131)
        self.assertEquals(result.median, 358817)
        self.assertAlmostEquals(result.sd, 8443.37, places=2)
        self.assertAlmostEquals(result.mean, 361463.25, places=2)
        self.assertEquals(result.num_samples, 8)
        samples = result.samples
        self.assertTrue(isinstance(samples, PerformanceTestSamples))
        self.assertEquals(samples.count, 8)

    def test_excludes_outliers_from_samples(self):
        verbose_log = """Running DropFirstAnySeqCntRangeLazy for 10 samples.
    Measuring with scale 2.
    Sample 0,455
    Measuring with scale 2.
    Sample 1,203
    Measuring with scale 2.
    Sample 2,205
    Measuring with scale 2.
    Sample 3,207
    Measuring with scale 2.
    Sample 4,208
    Measuring with scale 2.
    Sample 5,206
    Measuring with scale 2.
    Sample 6,205
    Measuring with scale 2.
    Sample 7,206
    Measuring with scale 2.
    Sample 8,208
    Measuring with scale 2.
    Sample 9,184
65,DropFirstAnySeqCntRangeLazy,10,184,455,228,79,206
"""
        parser = LogParser()
        result = parser.parse_results(verbose_log.split('\n'))[0]
        self.assertEquals(result.num_samples, 10)
        self.assertEquals(result.samples.count, 8)
        self.assertEquals(len(result.samples.outliers), 2)


class TestTestComparator(OldAndNewLog):
    def test_init(self):
        def names(tests):
            return [t.name for t in tests]

        tc = TestComparator(self.old_results, self.new_results, 0.05)
        self.assertEquals(names(tc.unchanged), ['AngryPhonebook', 'Array2D'])
        self.assertEquals(names(tc.increased), ['ByteSwap', 'ArrayAppend'])
        self.assertEquals(names(tc.decreased), ['BitCount'])
        self.assertEquals(names(tc.added), ['TwoSum'])
        self.assertEquals(names(tc.removed), ['AnyHashableWithAClass'])
        # other way around
        tc = TestComparator(self.new_results, self.old_results, 0.05)
        self.assertEquals(names(tc.unchanged), ['AngryPhonebook', 'Array2D'])
        self.assertEquals(names(tc.increased), ['BitCount'])
        self.assertEquals(names(tc.decreased), ['ByteSwap', 'ArrayAppend'])
        self.assertEquals(names(tc.added), ['AnyHashableWithAClass'])
        self.assertEquals(names(tc.removed), ['TwoSum'])
        # delta_threshold determines the sorting into change groups;
        # report only change above 100% (ByteSwap's runtime went to 0):
        tc = TestComparator(self.old_results, self.new_results, 1)
        self.assertEquals(
            names(tc.unchanged),
            ['AngryPhonebook', 'Array2D', 'ArrayAppend', 'BitCount']
        )
        self.assertEquals(names(tc.increased), ['ByteSwap'])
        self.assertEquals(tc.decreased, [])


class TestReportFormatter(OldAndNewLog):
    def setUp(self):
        super(TestReportFormatter, self).setUp()
        self.tc = TestComparator(self.old_results, self.new_results, 0.05)
        self.rf = ReportFormatter(self.tc, '', '', changes_only=False)
        self.markdown = self.rf.markdown()
        self.git = self.rf.git()
        self.html = self.rf.html()

    def assert_markdown_contains(self, texts):
        self.assert_report_contains(texts, self.markdown)

    def assert_git_contains(self, texts):
        self.assert_report_contains(texts, self.git)

    def assert_html_contains(self, texts):
        self.assert_report_contains(texts, self.html)

    def test_values(self):
        self.assertEquals(
            ReportFormatter.values(PerformanceTestResult(
                '1,AngryPhonebook,20,10664,12933,11035,576,10884'.split(','))),
            ('AngryPhonebook', '10664', '12933', '11035', '—')
        )
        self.assertEquals(
            ReportFormatter.values(PerformanceTestResult(
                '1,AngryPhonebook,1,12045,12045,12045,0,12045,10510336'
                .split(','))),
            ('AngryPhonebook', '12045', '12045', '12045', '10510336')
        )

        r1 = PerformanceTestResult(
            '1,AngryPhonebook,1,12325,12325,12325,0,12325,10510336'.split(','))
        r2 = PerformanceTestResult(
            '1,AngryPhonebook,1,11616,11616,11616,0,11616,10502144'.split(','))
        self.assertEquals(
            ReportFormatter.values(ResultComparison(r1, r2)),
            ('AngryPhonebook', '12325', '11616', '-5.8%', '1.06x')
        )
        self.assertEquals(
            ReportFormatter.values(ResultComparison(r2, r1)),
            ('AngryPhonebook', '11616', '12325', '+6.1%', '0.94x')
        )
        r2.max = r1.min + 1
        self.assertEquals(
            ReportFormatter.values(ResultComparison(r1, r2))[4],
            '1.06x (?)'  # is_dubious
        )

    def test_justified_columns(self):
        """Table columns are all formated with same width, defined by the
        longest value.
        """
        self.assert_markdown_contains([
            'AnyHashableWithAClass | 247027 | 319065 | 259056  | 10250445',
            'Array2D               | 335831 | 335831 | +0.0%   | 1.00x'])
        self.assert_git_contains([
            'AnyHashableWithAClass   247027   319065   259056    10250445',
            'Array2D                 335831   335831   +0.0%     1.00x'])

    def test_column_headers(self):
        """Report contains table headers for ResultComparisons and changed
        PerformanceTestResults.
        """
        performance_test_result = self.tc.added[0]
        self.assertEquals(
            ReportFormatter.header_for(performance_test_result),
            ('TEST', 'MIN', 'MAX', 'MEAN', 'MAX_RSS')
        )
        comparison_result = self.tc.increased[0]
        self.assertEquals(
            ReportFormatter.header_for(comparison_result),
            ('TEST', 'OLD', 'NEW', 'DELTA', 'RATIO')
        )
        self.assert_markdown_contains([
            'TEST                  | OLD    | NEW    | DELTA   | RATIO',
            '---                   | ---    | ---    | ---     | ---    ',
            'TEST                  | MIN    | MAX    | MEAN    | MAX_RSS'])
        self.assert_git_contains([
            'TEST                    OLD      NEW      DELTA     RATIO',
            'TEST                    MIN      MAX      MEAN      MAX_RSS'])
        self.assert_html_contains([
            """
                <th align='left'>OLD</th>
                <th align='left'>NEW</th>
                <th align='left'>DELTA</th>
                <th align='left'>RATIO</th>""",
            """
                <th align='left'>MIN</th>
                <th align='left'>MAX</th>
                <th align='left'>MEAN</th>
                <th align='left'>MAX_RSS</th>"""])

    def test_emphasize_speedup(self):
        """Emphasize speedup values for regressions and improvements"""
        # tests in No Changes don't have emphasized speedup
        self.assert_markdown_contains([
            'BitCount              | 3      | 9      | +199.9% | **0.33x**',
            'ByteSwap              | 4      | 0      | -100.0% | **4001.00x**',
            'AngryPhonebook        | 10458  | 10458  | +0.0%   | 1.00x ',
            'ArrayAppend           | 23641  | 20000  | -15.4%  | **1.18x (?)**'
        ])
        self.assert_git_contains([
            'BitCount                3        9        +199.9%   **0.33x**',
            'ByteSwap                4        0        -100.0%   **4001.00x**',
            'AngryPhonebook          10458    10458    +0.0%     1.00x',
            'ArrayAppend             23641    20000    -15.4%    **1.18x (?)**'
        ])
        self.assert_html_contains([
            """
        <tr>
                <td align='left'>BitCount</td>
                <td align='left'>3</td>
                <td align='left'>9</td>
                <td align='left'>+199.9%</td>
                <td align='left'><font color='red'>0.33x</font></td>
        </tr>""",
            """
        <tr>
                <td align='left'>ByteSwap</td>
                <td align='left'>4</td>
                <td align='left'>0</td>
                <td align='left'>-100.0%</td>
                <td align='left'><font color='green'>4001.00x</font></td>
        </tr>""",
            """
        <tr>
                <td align='left'>AngryPhonebook</td>
                <td align='left'>10458</td>
                <td align='left'>10458</td>
                <td align='left'>+0.0%</td>
                <td align='left'><font color='black'>1.00x</font></td>
        </tr>"""
        ])

    def test_sections(self):
        """Report is divided into sections with summaries."""
        self.assert_markdown_contains([
            """<details open>
  <summary>Regression (1)</summary>""",
            """<details >
  <summary>Improvement (2)</summary>""",
            """<details >
  <summary>No Changes (2)</summary>""",
            """<details open>
  <summary>Added (1)</summary>""",
            """<details open>
  <summary>Removed (1)</summary>"""])
        self.assert_git_contains([
            'Regression (1): \n',
            'Improvement (2): \n',
            'No Changes (2): \n',
            'Added (1): \n',
            'Removed (1): \n'])
        self.assert_html_contains([
            "<th align='left'>Regression (1)</th>",
            "<th align='left'>Improvement (2)</th>",
            "<th align='left'>No Changes (2)</th>",
            "<th align='left'>Added (1)</th>",
            "<th align='left'>Removed (1)</th>"])

    def test_report_only_changes(self):
        """Leave out tests without significant change."""
        rf = ReportFormatter(self.tc, '', '', changes_only=True)
        markdown, git, html = rf.markdown(), rf.git(), rf.html()
        self.assertNotIn('No Changes', markdown)
        self.assertNotIn('AngryPhonebook', markdown)
        self.assertNotIn('No Changes', git)
        self.assertNotIn('AngryPhonebook', git)
        self.assertNotIn('No Changes', html)
        self.assertNotIn('AngryPhonebook', html)


class Test_parse_args(unittest.TestCase):
    required = ['--old-file', 'old.log', '--new-file', 'new.log']

    def test_required_input_arguments(self):
        with captured_output() as (_, err):
            self.assertRaises(SystemExit, parse_args, [])
        self.assertIn('usage: compare_perf_tests.py', err.getvalue())

        args = parse_args(self.required)
        self.assertEquals(args.old_file, 'old.log')
        self.assertEquals(args.new_file, 'new.log')

    def test_format_argument(self):
        self.assertEquals(parse_args(self.required).format, 'markdown')
        self.assertEquals(
            parse_args(self.required + ['--format', 'markdown']).format,
            'markdown')
        self.assertEquals(
            parse_args(self.required + ['--format', 'git']).format, 'git')
        self.assertEquals(
            parse_args(self.required + ['--format', 'html']).format, 'html')

        with captured_output() as (_, err):
            self.assertRaises(SystemExit, parse_args,
                              self.required + ['--format', 'bogus'])
        self.assertIn("error: argument --format: invalid choice: 'bogus' "
                      "(choose from 'markdown', 'git', 'html')",
                      err.getvalue())

    def test_delta_threshold_argument(self):
        # default value
        args = parse_args(self.required)
        self.assertEquals(args.delta_threshold, 0.05)
        # float parsing
        args = parse_args(self.required + ['--delta-threshold', '0.1'])
        self.assertEquals(args.delta_threshold, 0.1)
        args = parse_args(self.required + ['--delta-threshold', '1'])
        self.assertEquals(args.delta_threshold, 1.0)
        args = parse_args(self.required + ['--delta-threshold', '.2'])
        self.assertEquals(args.delta_threshold, 0.2)

        with captured_output() as (_, err):
            self.assertRaises(SystemExit, parse_args,
                              self.required + ['--delta-threshold', '2,2'])
        self.assertIn(" error: argument --delta-threshold: invalid float "
                      "value: '2,2'",
                      err.getvalue())

    def test_output_argument(self):
        self.assertEquals(parse_args(self.required).output, None)
        self.assertEquals(parse_args(self.required +
                                     ['--output', 'report.log']).output,
                          'report.log')

    def test_changes_only_argument(self):
        self.assertFalse(parse_args(self.required).changes_only)
        self.assertTrue(parse_args(self.required +
                                   ['--changes-only']).changes_only)

    def test_branch_arguments(self):
        # default value
        args = parse_args(self.required)
        self.assertEquals(args.new_branch, 'NEW_MIN')
        self.assertEquals(args.old_branch, 'OLD_MIN')
        # user specified
        args = parse_args(
            self.required + ['--old-branch', 'master',
                             '--new-branch', 'amazing-optimization'])
        self.assertEquals(args.old_branch, 'master')
        self.assertEquals(args.new_branch, 'amazing-optimization')


class Test_compare_perf_tests_main(OldAndNewLog, FileSystemIntegration):
    """Integration test that invokes the whole comparison script."""
    markdown = [
        '<summary>Regression (1)</summary>',
        'TEST                  | OLD    | NEW    | DELTA   | RATIO',
        'BitCount              | 3      | 9      | +199.9% | **0.33x**',
    ]
    git = [
        'Regression (1):',
        'TEST                    OLD      NEW      DELTA     RATIO',
        'BitCount                3        9        +199.9%   **0.33x**',
    ]
    html = ['<html>', "<td align='left'>BitCount</td>"]

    def setUp(self):
        super(Test_compare_perf_tests_main, self).setUp()
        self.old_log = self.write_temp_file('old.log', self.old_log_content)
        self.new_log = self.write_temp_file('new.log', self.new_log_content)

    def execute_main_with_format(self, report_format, test_output=False):
        report_file = self.test_dir + 'report.log'
        args = ['compare_perf_tests.py',
                '--old-file', self.old_log,
                '--new-file', self.new_log,
                '--format', report_format]

        sys.argv = (args if not test_output else
                    args + ['--output', report_file])

        with captured_output() as (out, _):
            main()
        report_out = out.getvalue()

        if test_output:
            with open(report_file, 'r') as f:
                report = f.read()
            # because print adds newline, add one here, too:
            report_file = str(report + '\n')
        else:
            report_file = None

        return report_out, report_file

    def test_markdown(self):
        """Writes Markdown formatted report to stdout"""
        report_out, _ = self.execute_main_with_format('markdown')
        self.assert_report_contains(self.markdown, report_out)

    def test_markdown_output(self):
        """Writes Markdown formatted report to stdout and `--output` file."""
        report_out, report_file = (
            self.execute_main_with_format('markdown', test_output=True))
        self.assertEquals(report_out, report_file)
        self.assert_report_contains(self.markdown, report_file)

    def test_git(self):
        """Writes Git formatted report to stdout."""
        report_out, _ = self.execute_main_with_format('git')
        self.assert_report_contains(self.git, report_out)

    def test_git_output(self):
        """Writes Git formatted report to stdout and `--output` file."""
        report_out, report_file = (
            self.execute_main_with_format('git', test_output=True))
        self.assertEquals(report_out, report_file)
        self.assert_report_contains(self.git, report_file)

    def test_html(self):
        """Writes HTML formatted report to stdout."""
        report_out, _ = self.execute_main_with_format('html')
        self.assert_report_contains(self.html, report_out)

    def test_html_output(self):
        """Writes HTML formatted report to stdout and `--output` file."""
        report_out, report_file = (
            self.execute_main_with_format('html', test_output=True))
        self.assertEquals(report_out, report_file)
        self.assert_report_contains(self.html, report_file)


if __name__ == '__main__':
    unittest.main()
