#!/usr/bin/python
# -*- coding: utf-8 -*-

# ===--- benchmark_utils.py ----------------------------------------------===//
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
import tempfile
import unittest

from compare_perf_tests import PerformanceTestResult
from compare_perf_tests import ResultComparison
from compare_perf_tests import TestComparator


class TestPerformanceTestResult(unittest.TestCase):

    def test_init(self):
        log_line = '1,AngryPhonebook,20,10664,12933,11035,576,10884'
        r = PerformanceTestResult(log_line.split(','))
        self.assertEquals(r.name, 'AngryPhonebook')
        self.assertEquals((r.samples, r.min, r.max, r.mean, r.sd, r.median),
                          (20, 10664, 12933, 11035, 576, 10884))

        log_line = '1,AngryPhonebook,1,12045,12045,12045,0,12045,10510336'
        r = PerformanceTestResult(log_line.split(','))
        self.assertEquals(r.max_rss, 10510336)

    def test_repr(self):
        log_line = '1,AngryPhonebook,20,10664,12933,11035,576,10884'
        r = PerformanceTestResult(log_line.split(','))
        self.assertEquals(
            str(r),
            '<PerformanceTestResult name:\'AngryPhonebook\' samples:20 '
            'min:10664 max:12933 mean:11035 sd:576.0 median:10884>'
        )

    def test_header(self):
        self.assertEquals(PerformanceTestResult.header,
                          ('TEST', 'MIN', 'MAX', 'MEAN', 'MAX_RSS'))

    def test_values(self):
        log_line = '1,AngryPhonebook,20,10664,12933,11035,576,10884'
        r = PerformanceTestResult(log_line.split(','))
        self.assertEquals(
            r.values(),
            ('AngryPhonebook', '10664', '12933', '11035', '—')
        )
        log_line = '1,AngryPhonebook,1,12045,12045,12045,0,12045,10510336'
        r = PerformanceTestResult(log_line.split(','))
        self.assertEquals(
            r.values(),
            ('AngryPhonebook', '12045', '12045', '12045', '10510336')
        )

    def test_merge(self):
        tests = """1,AngryPhonebook,1,12045,12045,12045,0,12045,10510336
1,AngryPhonebook,1,12325,12325,12325,0,12325,10510336
1,AngryPhonebook,1,11616,11616,11616,0,11616,10502144
1,AngryPhonebook,1,12270,12270,12270,0,12270,10498048""".split('\n')
        results = map(PerformanceTestResult,
                      [line.split(',') for line in tests])

        def as_tuple(r):
            return (r.min, r.max, round(r.mean, 2), round(r.sd, 2), r.median,
                    r.max_rss)

        r = results[0]
        self.assertEquals(as_tuple(r),
                          (12045, 12045, 12045, 0, 12045, 10510336))
        r.merge(results[1])
        self.assertEquals(as_tuple(r),
                          (12045, 12325, 12185, 197.99, 12045, 10510336))
        r.merge(results[2])
        self.assertEquals(as_tuple(r),
                          (11616, 12325, 11995.33, 357.10, 12045, 10510336))
        r.merge(results[3])
        self.assertEquals(as_tuple(r),
                          (11616, 12325, 12064, 322.29, 12045, 10510336))


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

    def test_header(self):
        self.assertEquals(ResultComparison.header,
                          ('TEST', 'OLD', 'NEW', 'DELTA', 'SPEEDUP'))

    def test_values(self):
        rc = ResultComparison(self.r1, self.r2)
        self.assertEquals(
            rc.values(),
            ('AngryPhonebook', '12325', '11616', '-5.8%', '1.06x')
        )
        # other way around
        rc = ResultComparison(self.r2, self.r1)
        self.assertEquals(
            rc.values(),
            ('AngryPhonebook', '11616', '12325', '+6.1%', '0.94x')
        )

    def test_values_is_dubious(self):
        self.r2.max = self.r1.min + 1
        # new.min < old.min < new.max
        rc = ResultComparison(self.r1, self.r2)
        self.assertEquals(rc.values()[4], '1.06x (?)')
        # other way around: old.min < new.min < old.max
        rc = ResultComparison(self.r2, self.r1)
        self.assertEquals(rc.values()[4], '0.94x (?)')


class OldAndNewLog(unittest.TestCase):
    def setUp(self):
        # Create a temporary directory
        self.test_dir = tempfile.mkdtemp()

        self.old_log = self.write_temp_file('old.log', """
1,AngryPhonebook,20,10458,12714,11000,0,11000,10204365
2,AnyHashableWithAClass,20,247027,319065,259056,0,259056,10250445
3,Array2D,20,335831,400221,346622,0,346622,28297216
4,ArrayAppend,20,23641,29000,24990,0,24990,11149926
34,BitCount,20,3,4,4,0,4,10192896
35,ByteSwap,20,4,6,4,0,4,10185933

Totals,269,67351871,70727022,68220188,0,0,0
""")
        # Log lines are deliberately in descending sort order in order to test
        # the ordering of the comparison results.
        # ArrayAppend is included twice to test tesults merging.
        self.new_log = self.write_temp_file('new.log', """
265,TwoSum,20,5006,5679,5111,0,5111
35,ByteSwap,20,0,0,0,0,0
34,BitCount,20,9,9,9,0,9
4,ArrayAppend,20,23641,29000,24990,0,24990
3,Array2D,20,335831,400221,346622,0,346622
1,AngryPhonebook,20,10458,12714,11000,0,11000

Totals,269,67351871,70727022,68220188,0,0,0
4,ArrayAppend,1,20000,20000,20000,0,20000

Totals,1,20000,20000,20000,0,0,0
""")

    def tearDown(self):
        # Remove the directory after the test
        shutil.rmtree(self.test_dir)

    def write_temp_file(self, file_name, data):
        temp_file_name = os.path.join(self.test_dir, file_name)
        with open(temp_file_name, 'w') as f:
            f.write(data)
        return temp_file_name


class TestTestComparator(OldAndNewLog):
    def test_init(self):
        old_log, new_log = self.old_log, self.new_log

        def names(tests):
            return [t.name for t in tests]

        tc = TestComparator(old_log, new_log, 0.05)
        self.assertEquals(names(tc.unchanged), ['AngryPhonebook', 'Array2D'])
        self.assertEquals(names(tc.increased), ['ByteSwap', 'ArrayAppend'])
        self.assertEquals(names(tc.decreased), ['BitCount'])
        self.assertEquals(names(tc.added), ['TwoSum'])
        self.assertEquals(names(tc.removed), ['AnyHashableWithAClass'])
        # other way around
        tc = TestComparator(new_log, old_log, 0.05)
        self.assertEquals(names(tc.unchanged), ['AngryPhonebook', 'Array2D'])
        self.assertEquals(names(tc.increased), ['BitCount'])
        self.assertEquals(names(tc.decreased), ['ByteSwap', 'ArrayAppend'])
        self.assertEquals(names(tc.added), ['AnyHashableWithAClass'])
        self.assertEquals(names(tc.removed), ['TwoSum'])
        # delta_threshold determines the sorting into change groups;
        # report only change above 100% (ByteSwap's runtime went to 0):
        tc = TestComparator(old_log, new_log, 1)
        self.assertEquals(
            names(tc.unchanged),
            ['AngryPhonebook', 'Array2D', 'ArrayAppend', 'BitCount']
        )
        self.assertEquals(names(tc.increased), ['ByteSwap'])
        self.assertEquals(tc.decreased, [])


if __name__ == '__main__':
    unittest.main()
