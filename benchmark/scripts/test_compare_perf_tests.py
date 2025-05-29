#!/usr/bin/env python3
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

import json
import os
import shutil
import sys
import tempfile
import unittest

from compare_perf_tests import LogParser
from compare_perf_tests import PerformanceTestResult
from compare_perf_tests import ReportFormatter
from compare_perf_tests import ResultComparison
from compare_perf_tests import TestComparator
from compare_perf_tests import main
from compare_perf_tests import parse_args

from test_utils import captured_output


class TestPerformanceTestResult(unittest.TestCase):
    def test_init(self):
        header = "#,TEST,SAMPLES,MIN,MAX,MEAN,SD,MEDIAN"
        log_line = "1,AngryPhonebook,20,10664,12933,11035,576,10884"
        r = PerformanceTestResult.fromOldFormat(header, log_line)
        self.assertEqual(r.test_num, 1)
        self.assertEqual(r.name, "AngryPhonebook")
        self.assertEqual(
            (r.num_samples, r.min_value, r.max_value, r.mean, r.sd, r.median),
            (20, 10664, 12933, 11035, 576, 10884),
        )
        self.assertEqual(r.samples, [])

        header = "#,TEST,SAMPLES,MIN,MAX,MEAN,SD,MEDIAN,MAX_RSS"
        log_line = "1,AngryPhonebook,1,12045,12045,12045,0,12045,10510336"
        r = PerformanceTestResult.fromOldFormat(header, log_line)
        self.assertEqual(r.max_rss, 10510336)

    def test_init_quantiles(self):
        header = "#,TEST,SAMPLES,MIN(μs),MEDIAN(μs),MAX(μs)"
        log = "1,Ackermann,3,54383,54512,54601"
        r = PerformanceTestResult.fromQuantileFormat(header, log)
        self.assertEqual(r.test_num, 1)
        self.assertEqual(r.name, "Ackermann")
        self.assertEqual(
            (r.num_samples, r.min_value, r.median, r.max_value),
            (3, 54383, 54512, 54601)
        )
        self.assertAlmostEqual(r.mean, 54498.67, places=2)
        self.assertAlmostEqual(r.sd, 109.61, places=2)
        self.assertEqual(r.samples, [54383, 54512, 54601])

        header = "#,TEST,SAMPLES,MIN(μs),MEDIAN(μs),MAX(μs),MAX_RSS(B)"
        log = "1,Ackermann,3,54529,54760,55807,266240"
        r = PerformanceTestResult.fromQuantileFormat(header, log)
        self.assertEqual((len(r.samples), r.max_rss), (3, 266240))

        header = "#,TEST,SAMPLES,MIN(μs),Q1(μs),Q2(μs),Q3(μs),MAX(μs)"
        log = "1,Ackermann,5,54570,54593,54644,57212,58304"
        r = PerformanceTestResult.fromQuantileFormat(header, log)
        self.assertEqual(
            (r.num_samples, r.min_value, r.median, r.max_value),
            (5, 54570, 54644, 58304)
        )
        self.assertEqual((r.q1, r.q3), (54581.5, 57758))
        self.assertEqual(len(r.samples), 5)

        header = "#,TEST,SAMPLES,MIN(μs),Q1(μs),Q2(μs),Q3(μs),MAX(μs),MAX_RSS(B)"
        log = "1,Ackermann,5,54686,54731,54774,55030,63466,270336"
        r = PerformanceTestResult.fromQuantileFormat(header, log)
        self.assertEqual(r.num_samples, 5)
        self.assertEqual(len(r.samples), 5)
        self.assertEqual(r.max_rss, 270336)

    def test_init_delta_quantiles(self):
        # 2-quantile from 2 samples in repeated min, when delta encoded,
        # the difference is 0, which is omitted -- only separator remains
        header = "#,TEST,SAMPLES,MIN(μs),𝚫MEDIAN,𝚫MAX"
        log = "202,DropWhileArray,2,265,,22"
        r = PerformanceTestResult.fromQuantileFormat(header, log)
        self.assertEqual((r.num_samples, r.min_value, r.median, r.max_value),
                         (2, 265, 276, 287))
        self.assertEqual(len(r.samples), 2)
        self.assertEqual(r.num_samples, 2)

    def test_init_oversampled_quantiles(self):
        """When num_samples is < quantile + 1, some of the measurements are
        repeated in the report summary. Samples should contain only true
        values, discarding the repeated artifacts from quantile estimation.

        The test string is slightly massaged output of the following R script:
        subsample <- function(x, q) {
          quantile(1:x, probs=((0:(q-1))/(q-1)), type=1)}
        tbl <- function(s) t(sapply(1:s, function(x) {
          qs <- subsample(x, s); c(qs[1], diff(qs)) }))
        sapply(c(3, 5, 11, 21), tbl)

        TODO: Delete this test when we delete quantile support from the
        benchmark harness. Reconstructing samples from quantiles as this code is
        trying to do is not really statistically sound, which is why we're going
        to delete most of this in favor of an architecture where the
        lowest-level benchmarking logic reports samples, we store and pass
        raw sample data around as much as possible, and summary statistics are
        only computed as necessary for actual reporting (and then discarded,
        since we can recompute anything we need if we always have the raw
        samples available).
        """

        def validatePTR(deq):  # construct from delta encoded quantiles string
            deq = deq.split(",")
            num_samples = deq.count("1")
            r = PerformanceTestResult(
                ["0", "B", str(num_samples)] + deq, quantiles=True, delta=True
            )
            self.assertEqual(len(r.samples), num_samples)
            self.assertEqual(r.samples, range(1, num_samples + 1))

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
        map(validatePTR, delta_encoded_quantiles.split("\n")[1:])

    def test_init_meta(self):
        header = (
            "#,TEST,SAMPLES,MIN(μs),MAX(μs),MEAN(μs),SD(μs),"
            + "MEDIAN(μs),PAGES,ICS,YIELD"
        )
        log = "1,Ackermann,200,715,1281,726,47,715,7,29,15"
        r = PerformanceTestResult.fromOldFormat(header, log)
        self.assertEqual((r.test_num, r.name), (1, "Ackermann"))
        self.assertEqual(
            (r.num_samples, r.min_value, r.max_value, r.mean, r.sd, r.median),
            (200, 715, 1281, 726, 47, 715),
        )
        self.assertEqual((r.mem_pages, r.involuntary_cs, r.yield_count), (7, 29, 15))
        header = (
            "#,TEST,SAMPLES,MIN(μs),MAX(μs),MEAN(μs),SD(μs),MEDIAN(μs),"
            + "MAX_RSS(B),PAGES,ICS,YIELD"
        )
        log = "1,Ackermann,200,715,1951,734,97,715,36864,9,50,15"
        r = PerformanceTestResult.fromOldFormat(header, log)
        self.assertEqual(
            (r.num_samples, r.min_value, r.max_value, r.mean, r.sd, r.median),
            (200, 715, 1951, 734, 97, 715),
        )
        self.assertEqual(
            (r.mem_pages, r.involuntary_cs, r.yield_count, r.max_rss),
            (9, 50, 15, 36864),
        )
        header = "#,TEST,SAMPLES,MIN(μs),MAX(μs),PAGES,ICS,YIELD"
        log = "1,Ackermann,200,715,3548,8,31,15"
        r = PerformanceTestResult.fromOldFormat(header, log)
        self.assertEqual((r.num_samples, r.min_value, r.max_value), (200, 715, 3548))
        self.assertEqual(r.samples, [])
        self.assertEqual((r.mem_pages, r.involuntary_cs, r.yield_count), (8, 31, 15))

        header = "#,TEST,SAMPLES,MIN(μs),MAX(μs),MAX_RSS(B),PAGES,ICS,YIELD"
        log = "1,Ackermann,200,715,1259,32768,8,28,15"
        r = PerformanceTestResult.fromOldFormat(header, log)
        self.assertEqual((r.num_samples, r.min_value, r.max_value), (200, 715, 1259))
        self.assertEqual(r.samples, [])
        self.assertEqual(r.max_rss, 32768)
        self.assertEqual((r.mem_pages, r.involuntary_cs, r.yield_count), (8, 28, 15))

    def test_merge(self):
        tests = [
            """{"number":1,"name":"AngryPhonebook",
            "samples":[12045]}""",
            """{"number":1,"name":"AngryPhonebook",
            "samples":[12325],"max_rss":10510336}""",
            """{"number":1,"name":"AngryPhonebook",
            "samples":[11616],"max_rss":10502144}""",
            """{"number":1,"name":"AngryPhonebook",
            "samples":[12270],"max_rss":10498048}"""
        ]

        results = [PerformanceTestResult(json) for json in tests]

        def as_tuple(r):
            return (
                r.num_samples,
                r.min_value,
                r.max_value,
                round(r.mean, 2),
                round(r.sd, 2),
                r.median,
                r.max_rss,
            )

        r = results[0]
        self.assertEqual(as_tuple(r), (1, 12045, 12045, 12045, 0, 12045, None))
        r.merge(results[1])
        self.assertEqual(
            as_tuple(r),
            (2, 12045, 12325, 12185, 197.99, 12185, 10510336),
        )
        r.merge(results[2])
        self.assertEqual(
            as_tuple(r),
            (3, 11616, 12325, 11995.33, 357.1, 12045, 10502144),
        )
        r.merge(results[3])
        self.assertEqual(
            as_tuple(r),
            (4, 11616, 12325, 12064, 322.29, 12157.5, 10498048),
        )

    def test_legacy_merge(self):
        header = """#,TEST,NUM_SAMPLES,MIN,MAX,MEAN,SD,MEDIAN, MAX_RSS"""
        tests = [
            """1,AngryPhonebook,8,12045,12045,12045,0,12045""",
            """1,AngryPhonebook,8,12325,12325,12325,0,12325,10510336""",
            """1,AngryPhonebook,8,11616,11616,11616,0,11616,10502144""",
            """1,AngryPhonebook,8,12270,12270,12270,0,12270,10498048"""
        ]

        results = [PerformanceTestResult.fromOldFormat(header, row) for row in tests]

        def as_tuple(r):
            return (
                r.num_samples,
                r.min_value,
                r.max_value,
                round(r.mean, 2),
                round(r.sd, 2) if r.sd is not None else None,
                r.median,
                r.max_rss,
            )

        r = results[0]
        self.assertEqual(as_tuple(r), (8, 12045, 12045, 12045, 0, 12045, None))
        r.merge(results[1])
        self.assertEqual(
            as_tuple(r),  # Note: SD, Median are lost
            (16, 12045, 12325, 12185, None, None, 10510336),
        )
        r.merge(results[2])
        self.assertEqual(
            as_tuple(r),
            (24, 11616, 12325, 11995.33, None, None, 10502144),
        )
        r.merge(results[3])
        self.assertEqual(
            as_tuple(r),
            (32, 11616, 12325, 12064, None, None, 10498048),
        )


class TestResultComparison(unittest.TestCase):
    def setUp(self):
        self.r0 = PerformanceTestResult(
            """{"number":101,"name":"GlobalClass",
            "samples":[0,0,0,0,0],"max_rss":10185728}"""
        )
        self.r01 = PerformanceTestResult(
            """{"number":101,"name":"GlobalClass",
            "samples":[20,20,20],"max_rss":10185728}"""
        )
        self.r1 = PerformanceTestResult(
            """{"number":1,"name":"AngryPhonebook",
            "samples":[12325],"max_rss":10510336}"""
        )
        self.r2 = PerformanceTestResult(
            """{"number":1,"name":"AngryPhonebook",
            "samples":[11616],"max_rss":10502144}"""
        )
        self.r3 = PerformanceTestResult(
            """{"number":1,"name":"AngryPhonebook",
            "samples":[11616,12326],"max_rss":10502144}"""
        )

    def test_init(self):
        rc = ResultComparison(self.r1, self.r2)
        self.assertEqual(rc.name, "AngryPhonebook")
        self.assertAlmostEqual(rc.ratio, 12325.0 / 11616.0)
        self.assertAlmostEqual(rc.delta, (((11616.0 / 12325.0) - 1) * 100), places=3)
        # handle test results that sometimes change to zero, when compiler
        # optimizes out the body of the incorrectly written test
        rc = ResultComparison(self.r0, self.r0)
        self.assertEqual(rc.name, "GlobalClass")
        self.assertAlmostEqual(rc.ratio, 1)
        self.assertAlmostEqual(rc.delta, 0, places=3)
        rc = ResultComparison(self.r0, self.r01)
        self.assertAlmostEqual(rc.ratio, 0, places=3)
        self.assertAlmostEqual(rc.delta, 2000000, places=3)
        rc = ResultComparison(self.r01, self.r0)
        self.assertAlmostEqual(rc.ratio, 20001)
        self.assertAlmostEqual(rc.delta, -99.995, places=3)
        # disallow comparison of different test results
        self.assertRaises(AssertionError, ResultComparison, self.r0, self.r1)

    def test_values_is_dubious(self):
        self.assertFalse(ResultComparison(self.r1, self.r2).is_dubious)
        # new.min < old.min < new.max
        self.assertTrue(ResultComparison(self.r1, self.r3).is_dubious)
        # other way around: old.min < new.min < old.max
        self.assertTrue(ResultComparison(self.r3, self.r1).is_dubious)


class FileSystemIntegration(unittest.TestCase):
    def setUp(self):
        # Create a temporary directory
        self.test_dir = tempfile.mkdtemp()

    def tearDown(self):
        # Remove the directory after the test
        shutil.rmtree(self.test_dir)

    def write_temp_file(self, file_name, data):
        temp_file_name = os.path.join(self.test_dir, file_name)
        with open(temp_file_name, "w") as f:
            for line in data:
                f.write(line)
                f.write('\n')
        return temp_file_name


class OldAndNewLog(unittest.TestCase):

    old_log_content = [
        """{"number":1,"name":"AngryPhonebook","""
        + """"samples":[10458,12714,11000],"max_rss":10204365}""",
        """{"number":2,"name":"AnyHashableWithAClass","""
        + """"samples":[247027,319065,259056,259056],"max_rss":10250445}""",
        """{"number":3,"name":"Array2D","""
        + """"samples":[335831,400221,346622,346622],"max_rss":28297216}""",
        """{"number":4,"name":"ArrayAppend","""
        + """"samples":[23641,29000,24990,24990],"max_rss":11149926}""",
        """{"number":34,"name":"BitCount","samples":[3,4,4,4],"max_rss":10192896}""",
        """{"number":35,"name":"ByteSwap","samples":[4,6,4,4],"max_rss":10185933}"""
    ]

    new_log_content = [
        """{"number":265,"name":"TwoSum","samples":[5006,5679,5111,5111]}""",
        """{"number":35,"name":"ByteSwap","samples":[0,0,0,0,0]}""",
        """{"number":34,"name":"BitCount","samples":[9,9,9,9]}""",
        """{"number":4,"name":"ArrayAppend","samples":[20000,29000,24990,24990]}""",
        """{"number":3,"name":"Array2D","samples":[335831,400221,346622,346622]}""",
        """{"number":1,"name":"AngryPhonebook","samples":[10458,12714,11000,11000]}"""
    ]

    def makeResult(json_text):
        return PerformanceTestResult(json.loads(json_text))

    old_results = dict(
        [
            (r.name, r) for r in map(makeResult, old_log_content)
        ]
    )

    new_results = dict(
        [
            (r.name, r) for r in map(makeResult, new_log_content)
        ]
    )

    def assert_report_contains(self, texts, report):
        assert not isinstance(texts, str)
        for text in texts:
            self.assertIn(text, report)


class TestLogParser(unittest.TestCase):
    def test_parse_results_csv(self):
        """Ignores unknown lines, extracts data from supported formats."""
        log = """#,TEST,SAMPLES,MIN(us),MAX(us),MEAN(us),SD(us),MEDIAN(us)
7,Array.append.Array.Int?,20,10,10,10,0,10
21,Bridging.NSArray.as!.Array.NSString,20,11,11,11,0,11
42,Flatten.Array.Tuple4.lazy.for-in.Reserve,20,3,4,4,0,4

Total performance tests executed: 1
"""
        parser = LogParser()
        results = parser.parse_results(log.splitlines())
        self.assertTrue(isinstance(results[0], PerformanceTestResult))
        self.assertEqual(results[0].name, "Array.append.Array.Int?")
        self.assertEqual(results[1].name, "Bridging.NSArray.as!.Array.NSString")
        self.assertEqual(results[2].name, "Flatten.Array.Tuple4.lazy.for-in.Reserve")

    def test_parse_results_tab_delimited(self):
        log = "34\tBitCount\t20\t3\t4\t4\t0\t4"
        parser = LogParser()
        results = parser.parse_results(log.splitlines())
        self.assertTrue(isinstance(results[0], PerformanceTestResult))
        self.assertEqual(results[0].name, "BitCount")

    def test_parse_results_formatted_text(self):
        """Parse format that Benchmark_Driver prints to console"""
        log = """
  # TEST      SAMPLES MIN(μs) MAX(μs) MEAN(μs) SD(μs) MEDIAN(μs) MAX_RSS(B)
  3 Array2D        20    2060    2188     2099      0       2099   20915200

Total performance tests executed: 1
"""
        parser = LogParser()
        results = parser.parse_results(log.splitlines()[1:])  # without 1st \n
        self.assertTrue(isinstance(results[0], PerformanceTestResult))
        r = results[0]
        self.assertEqual(r.name, "Array2D")
        self.assertEqual(r.max_rss, 20915200)

    def test_parse_quantiles(self):
        """Gathers samples from reported quantiles. Handles optional memory."""
        r = LogParser.results_from_string(
            """#,TEST,SAMPLES,QMIN(μs),MEDIAN(μs),MAX(μs)
1,Ackermann,3,54383,54512,54601"""
        )["Ackermann"]
        self.assertEqual(r.samples, [54383, 54512, 54601])
        r = LogParser.results_from_string(
            """#,TEST,SAMPLES,QMIN(μs),MEDIAN(μs),MAX(μs),MAX_RSS(B)
1,Ackermann,3,54529,54760,55807,266240"""
        )["Ackermann"]
        self.assertEqual(r.samples, [54529, 54760, 55807])
        self.assertEqual(r.max_rss, 266240)

    def test_parse_delta_quantiles(self):
        r = LogParser.results_from_string(  # 2-quantile aka. median
            "#,TEST,SAMPLES,QMIN(μs),𝚫MEDIAN,𝚫MAX\n0,B,1,101,,"
        )["B"]
        self.assertEqual(
            (r.num_samples, r.min_value, r.median, r.max_value, len(r.samples)),
            (1, 101, 101, 101, 1),
        )
        r = LogParser.results_from_string(
            "#,TEST,SAMPLES,QMIN(μs),𝚫MEDIAN,𝚫MAX\n0,B,2,101,,1"
        )["B"]
        self.assertEqual(
            (r.num_samples, r.min_value, r.median, r.max_value, len(r.samples)),
            (2, 101, 101.5, 102, 2),
        )
        r = LogParser.results_from_string(  # 20-quantiles aka. ventiles
            "#,TEST,SAMPLES,QMIN(μs),𝚫V1,𝚫V2,𝚫V3,𝚫V4,𝚫V5,𝚫V6,𝚫V7,𝚫V8,"
            + "𝚫V9,𝚫VA,𝚫VB,𝚫VC,𝚫VD,𝚫VE,𝚫VF,𝚫VG,𝚫VH,𝚫VI,𝚫VJ,𝚫MAX\n"
            + "202,DropWhileArray,200,214,,,,,,,,,,,,1,,,,,,2,16,464"
        )["DropWhileArray"]
        self.assertEqual(
            (r.num_samples, r.min_value, r.max_value, len(r.samples)),
            (200, 214, 697, 0),
        )

    def test_parse_meta(self):
        r = LogParser.results_from_string(
            "#,TEST,SAMPLES,MIN(μs),MAX(μs),MEAN(μs),SD(μs),MEDIAN(μs),"
            + "PAGES,ICS,YIELD\n"
            + "0,B,1,2,2,2,0,2,7,29,15"
        )["B"]
        self.assertEqual(
            (r.min_value, r.mem_pages, r.involuntary_cs, r.yield_count), (2, 7, 29, 15)
        )
        r = LogParser.results_from_string(
            "#,TEST,SAMPLES,MIN(μs),MAX(μs),MEAN(μs),SD(μs),MEDIAN(μs),"
            + "MAX_RSS(B),PAGES,ICS,YIELD\n"
            + "0,B,1,3,3,3,0,3,36864,9,50,15"
        )["B"]
        self.assertEqual(
            (r.min_value, r.mem_pages, r.involuntary_cs, r.yield_count, r.max_rss),
            (3, 9, 50, 15, 36864),
        )
        r = LogParser.results_from_string(
            "#,TEST,SAMPLES,QMIN(μs),MAX(μs),PAGES,ICS,YIELD\n" + "0,B,1,4,4,8,31,15"
        )["B"]
        self.assertEqual(
            (r.min_value, r.mem_pages, r.involuntary_cs, r.yield_count), (4, 8, 31, 15)
        )
        r = LogParser.results_from_string(
            "#,TEST,SAMPLES,QMIN(μs),MAX(μs),MAX_RSS(B),PAGES,ICS,YIELD\n"
            + "0,B,1,5,5,32768,8,28,15"
        )["B"]
        self.assertEqual(
            (r.min_value, r.mem_pages, r.involuntary_cs, r.yield_count, r.max_rss),
            (5, 8, 28, 15, 32768),
        )

    def test_results_from_merge(self):
        """Parsing concatenated log merges same PerformanceTestResults"""
        concatenated_logs = """#,TEST,SAMPLES,MIN,MAX,MEAN,SD,MEDIAN
4,ArrayAppend,20,23641,29000,24990,0,24990
4,ArrayAppend,1,20000,20000,20000,0,20000"""
        results = LogParser.results_from_string(concatenated_logs)
        self.assertEqual(list(results.keys()), ["ArrayAppend"])
        result = results["ArrayAppend"]
        self.assertTrue(isinstance(result, PerformanceTestResult))
        self.assertEqual(result.min_value, 20000)
        self.assertEqual(result.max_value, 29000)


class TestTestComparator(OldAndNewLog):
    def test_init(self):
        def names(tests):
            return [t.name for t in tests]

        tc = TestComparator(self.old_results, self.new_results, 0.05)
        self.assertEqual(names(tc.unchanged), ["AngryPhonebook", "Array2D"])
#        self.assertEqual(names(tc.increased), ["ByteSwap", "ArrayAppend"])
        self.assertEqual(names(tc.decreased), ["BitCount"])
        self.assertEqual(names(tc.added), ["TwoSum"])
        self.assertEqual(names(tc.removed), ["AnyHashableWithAClass"])
        # other way around
        tc = TestComparator(self.new_results, self.old_results, 0.05)
        self.assertEqual(names(tc.unchanged), ["AngryPhonebook", "Array2D"])
        self.assertEqual(names(tc.increased), ["BitCount"])
        self.assertEqual(names(tc.decreased), ["ByteSwap", "ArrayAppend"])
        self.assertEqual(names(tc.added), ["AnyHashableWithAClass"])
        self.assertEqual(names(tc.removed), ["TwoSum"])
        # delta_threshold determines the sorting into change groups;
        # report only change above 100% (ByteSwap's runtime went to 0):
        tc = TestComparator(self.old_results, self.new_results, 1)
        self.assertEqual(
            names(tc.unchanged),
            ["AngryPhonebook", "Array2D", "ArrayAppend", "BitCount"],
        )
        self.assertEqual(names(tc.increased), ["ByteSwap"])
        self.assertEqual(tc.decreased, [])


class TestReportFormatter(OldAndNewLog):
    def setUp(self):
        super(TestReportFormatter, self).setUp()
        self.tc = TestComparator(self.old_results, self.new_results, 0.05)
        self.rf = ReportFormatter(self.tc, changes_only=False)
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
        self.assertEqual(
            ReportFormatter.values(
                PerformanceTestResult(
                    """{"number":1,"name":"AngryPhonebook",
                    "samples":[10664,12933,11035,10884]}"""
                )
            ),
            ("AngryPhonebook", "10664", "12933", "11379", "—"),
        )
        self.assertEqual(
            ReportFormatter.values(
                PerformanceTestResult(
                    """{"number":1,"name":"AngryPhonebook",
                    "samples":[12045],"max_rss":10510336}"""
                )
            ),
            ("AngryPhonebook", "12045", "12045", "12045", "10510336"),
        )

        r1 = PerformanceTestResult(
            """{"number":1,"name":"AngryPhonebook",
            "samples":[12325],"max_rss":10510336}"""
        )
        r2 = PerformanceTestResult(
            """{"number":1,"name":"AngryPhonebook",
            "samples":[11616],"max_rss":10510336}"""
        )
        self.assertEqual(
            ReportFormatter.values(ResultComparison(r1, r2)),
            ("AngryPhonebook", "12325", "11616", "-5.8%", "1.06x"),
        )
        self.assertEqual(
            ReportFormatter.values(ResultComparison(r2, r1)),
            ("AngryPhonebook", "11616", "12325", "+6.1%", "0.94x"),
        )

        r1 = PerformanceTestResult(
            """{"number":1,"name":"AngryPhonebook",
            "samples":[12325],"max_rss":10510336}"""
        )
        r2 = PerformanceTestResult(
            """{"number":1,"name":"AngryPhonebook",
            "samples":[11616,12326],"max_rss":10510336}"""
        )
        self.assertEqual(
            ReportFormatter.values(ResultComparison(r1, r2))[4],
            "1.06x (?)",  # is_dubious
        )

    def test_justified_columns(self):
        """Table columns are all formatted with same width, defined by the
        longest value.
        """
        self.assert_markdown_contains(
            [
                "AnyHashableWithAClass | 247027 | 319065 | 271051  | 10250445",
                "Array2D               | 335831 | 335831 | +0.0%   | 1.00x",
            ]
        )
        self.assert_git_contains(
            [
                "AnyHashableWithAClass   247027   319065   271051    10250445",
                "Array2D                 335831   335831   +0.0%     1.00x",
            ]
        )

    def test_column_headers(self):
        """Report contains table headers for ResultComparisons and changed
        PerformanceTestResults.
        """
        performance_test_result = self.tc.added[0]
        self.assertEqual(
            ReportFormatter.header_for(performance_test_result),
            ("TEST", "MIN", "MAX", "MEAN", "MAX_RSS"),
        )
        comparison_result = self.tc.increased[0]
        self.assertEqual(
            ReportFormatter.header_for(comparison_result),
            ("TEST", "OLD", "NEW", "DELTA", "RATIO"),
        )
        self.assert_markdown_contains(
            [
                "TEST                  | OLD    | NEW    | DELTA   | RATIO",
                ":---                  | ---:   | ---:   | ---:    | ---:   ",
                "TEST                  | MIN    | MAX    | MEAN    | MAX_RSS",
            ]
        )
        self.assert_git_contains(
            [
                "TEST                    OLD      NEW      DELTA     RATIO",
                "TEST                    MIN      MAX      MEAN      MAX_RSS",
            ]
        )
        self.assert_html_contains(
            [
                """
                <th align='left'>OLD</th>
                <th align='left'>NEW</th>
                <th align='left'>DELTA</th>
                <th align='left'>RATIO</th>""",
                """
                <th align='left'>MIN</th>
                <th align='left'>MAX</th>
                <th align='left'>MEAN</th>
                <th align='left'>MAX_RSS</th>""",
            ]
        )

    def test_emphasize_speedup(self):
        """Emphasize speedup values for regressions and improvements"""
        # tests in No Changes don't have emphasized speedup
        self.assert_markdown_contains(
            [
                "BitCount              | 3      | 9      | +199.9% | **0.33x**",
                "ByteSwap              | 4      | 0      | -100.0% | **4001.00x**",
                "AngryPhonebook        | 10458  | 10458  | +0.0%   | 1.00x ",
                "ArrayAppend           | 23641  | 20000  | -15.4%  | **1.18x (?)**",
            ]
        )
        self.assert_git_contains(
            [
                "BitCount                3        9        +199.9%   **0.33x**",
                "ByteSwap                4        0        -100.0%   **4001.00x**",
                "AngryPhonebook          10458    10458    +0.0%     1.00x",
                "ArrayAppend             23641    20000    -15.4%    **1.18x (?)**",
            ]
        )
        self.assert_html_contains(
            [
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
        </tr>""",
            ]
        )

    def test_sections(self):
        """Report is divided into sections with summaries."""
        self.assert_markdown_contains(
            [
                """<details open>
  <summary>Regression (1)</summary>""",
                """<details >
  <summary>Improvement (2)</summary>""",
                """<details >
  <summary>No Changes (2)</summary>""",
                """<details open>
  <summary>Added (1)</summary>""",
                """<details open>
  <summary>Removed (1)</summary>""",
            ]
        )
        self.assert_git_contains(
            [
                "Regression (1): \n",
                "Improvement (2): \n",
                "No Changes (2): \n",
                "Added (1): \n",
                "Removed (1): \n",
            ]
        )
        self.assert_html_contains(
            [
                "<th align='left'>Regression (1)</th>",
                "<th align='left'>Improvement (2)</th>",
                "<th align='left'>No Changes (2)</th>",
                "<th align='left'>Added (1)</th>",
                "<th align='left'>Removed (1)</th>",
            ]
        )

    def test_report_only_changes(self):
        """Leave out tests without significant change."""
        rf = ReportFormatter(self.tc, changes_only=True)
        markdown, git, html = rf.markdown(), rf.git(), rf.html()
        self.assertNotIn("No Changes", markdown)
        self.assertNotIn("AngryPhonebook", markdown)
        self.assertNotIn("No Changes", git)
        self.assertNotIn("AngryPhonebook", git)
        self.assertNotIn("No Changes", html)
        self.assertNotIn("AngryPhonebook", html)

    def test_single_table_report(self):
        """Single table report has inline headers and no elaborate sections."""
        self.tc.removed = []  # test handling empty section
        rf = ReportFormatter(self.tc, changes_only=True, single_table=True)
        markdown = rf.markdown()
        self.assertNotIn("<details", markdown)  # no sections
        self.assertNotIn("\n\n", markdown)  # table must not be broken
        self.assertNotIn("Removed", markdown)
        self.assert_report_contains(
            [
                "\n**Regression** ",
                "| **OLD**",
                "| **NEW**",
                "| **DELTA**",
                "| **RATIO**",
                "\n**Added** ",
                "| **MIN**",
                "| **MAX**",
                "| **MEAN**",
                "| **MAX_RSS**",
            ],
            markdown,
        )
        # Single delimiter row:
        self.assertIn("\n:---", markdown)  # first column is left aligned
        self.assertEqual(markdown.count("| ---:"), 4)  # other, right aligned
        # Separator before every inline header (new section):
        self.assertEqual(markdown.count("&nbsp; | | | | "), 2)

        git = rf.git()
        self.assertNotIn("): \n", git)  # no sections
        self.assertNotIn("REMOVED", git)
        self.assert_report_contains(
            [
                "\nREGRESSION ",
                " OLD ",
                " NEW ",
                " DELTA ",
                " RATIO ",
                "\n\nADDED ",
                " MIN ",
                " MAX ",
                " MEAN ",
                " MAX_RSS ",
            ],
            git,
        )
        # Separator before every inline header (new section):
        self.assertEqual(git.count("\n\n"), 2)


class Test_parse_args(unittest.TestCase):
    required = ["--old-file", "old.log", "--new-file", "new.log"]

    def test_required_input_arguments(self):
        with captured_output() as (_, err):
            self.assertRaises(SystemExit, parse_args, [])
        self.assertIn("usage: compare_perf_tests.py", err.getvalue())

        args = parse_args(self.required)
        self.assertEqual(args.old_file, "old.log")
        self.assertEqual(args.new_file, "new.log")

    def test_format_argument(self):
        self.assertEqual(parse_args(self.required).format, "markdown")
        self.assertEqual(
            parse_args(self.required + ["--format", "markdown"]).format, "markdown"
        )
        self.assertEqual(parse_args(self.required + ["--format", "git"]).format, "git")
        self.assertEqual(
            parse_args(self.required + ["--format", "html"]).format, "html"
        )

        with captured_output() as (_, err):
            self.assertRaises(
                SystemExit, parse_args, self.required + ["--format", "bogus"]
            )
        self.assertIn(
            "error: argument --format: invalid choice: 'bogus' "
            "(choose from 'markdown', 'git', 'html')",
            err.getvalue(),
        )

    def test_delta_threshold_argument(self):
        # default value
        args = parse_args(self.required)
        self.assertEqual(args.delta_threshold, 0.05)
        # float parsing
        args = parse_args(self.required + ["--delta-threshold", "0.1"])
        self.assertEqual(args.delta_threshold, 0.1)
        args = parse_args(self.required + ["--delta-threshold", "1"])
        self.assertEqual(args.delta_threshold, 1.0)
        args = parse_args(self.required + ["--delta-threshold", ".2"])
        self.assertEqual(args.delta_threshold, 0.2)

        with captured_output() as (_, err):
            self.assertRaises(
                SystemExit, parse_args, self.required + ["--delta-threshold", "2,2"]
            )
        self.assertIn(
            " error: argument --delta-threshold: invalid float " "value: '2,2'",
            err.getvalue(),
        )

    def test_output_argument(self):
        self.assertEqual(parse_args(self.required).output, None)
        self.assertEqual(
            parse_args(self.required + ["--output", "report.log"]).output, "report.log"
        )

    def test_changes_only_argument(self):
        self.assertFalse(parse_args(self.required).changes_only)
        self.assertTrue(parse_args(self.required + ["--changes-only"]).changes_only)


class Test_compare_perf_tests_main(OldAndNewLog, FileSystemIntegration):
    """Integration test that invokes the whole comparison script."""

    markdown = [
        "<summary>Regression (1)</summary>",
        "TEST                  | OLD    | NEW    | DELTA   | RATIO",
        "BitCount              | 3      | 9      | +199.9% | **0.33x**",
    ]
    git = [
        "Regression (1):",
        "TEST                    OLD      NEW      DELTA     RATIO",
        "BitCount                3        9        +199.9%   **0.33x**",
    ]
    html = ["<html>", "<td align='left'>BitCount</td>"]

    def setUp(self):
        super(Test_compare_perf_tests_main, self).setUp()
        self.old_log = self.write_temp_file("old.log", self.old_log_content)
        self.new_log = self.write_temp_file("new.log", self.new_log_content)

    def execute_main_with_format(self, report_format, test_output=False):
        report_file = self.test_dir + "report.log"
        args = [
            "compare_perf_tests.py",
            "--old-file",
            self.old_log,
            "--new-file",
            self.new_log,
            "--format",
            report_format,
        ]

        sys.argv = args if not test_output else args + ["--output", report_file]

        with captured_output() as (out, _):
            main()
        report_out = out.getvalue()

        if test_output:
            with open(report_file, "r") as f:
                report = f.read()
            # because print adds newline, add one here, too:
            report_file = str(report + "\n")
        else:
            report_file = None

        return report_out, report_file

    def test_markdown(self):
        """Writes Markdown formatted report to stdout"""
        report_out, _ = self.execute_main_with_format("markdown")
        self.assert_report_contains(self.markdown, report_out)

    def test_markdown_output(self):
        """Writes Markdown formatted report to stdout and `--output` file."""
        report_out, report_file = self.execute_main_with_format(
            "markdown", test_output=True
        )
        self.assertEqual(report_out, report_file)
        self.assert_report_contains(self.markdown, report_file)

    def test_git(self):
        """Writes Git formatted report to stdout."""
        report_out, _ = self.execute_main_with_format("git")
        self.assert_report_contains(self.git, report_out)

    def test_git_output(self):
        """Writes Git formatted report to stdout and `--output` file."""
        report_out, report_file = self.execute_main_with_format("git", test_output=True)
        self.assertEqual(report_out, report_file)
        self.assert_report_contains(self.git, report_file)

    def test_html(self):
        """Writes HTML formatted report to stdout."""
        report_out, _ = self.execute_main_with_format("html")
        self.assert_report_contains(self.html, report_out)

    def test_html_output(self):
        """Writes HTML formatted report to stdout and `--output` file."""
        report_out, report_file = self.execute_main_with_format(
            "html", test_output=True
        )
        self.assertEqual(report_out, report_file)
        self.assert_report_contains(self.html, report_file)


if __name__ == "__main__":
    unittest.main()
