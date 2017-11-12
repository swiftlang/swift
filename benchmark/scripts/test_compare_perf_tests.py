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

from StringIO import StringIO
from contextlib import contextmanager

from compare_perf_tests import PerformanceTestResult
from compare_perf_tests import ReportFormatter
from compare_perf_tests import ResultComparison
from compare_perf_tests import TestComparator
from compare_perf_tests import main
from compare_perf_tests import parse_args


@contextmanager
def captured_output():
    new_out, new_err = StringIO(), StringIO()
    old_out, old_err = sys.stdout, sys.stderr
    try:
        sys.stdout, sys.stderr = new_out, new_err
        yield sys.stdout, sys.stderr
    finally:
        sys.stdout, sys.stderr = old_out, old_err


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
        # ArrayAppend is included twice to test results merging.
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

    def assert_report_contains(self, texts, report):
        assert not isinstance(texts, str)
        for text in texts:
            self.assertIn(text, report)


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


class TestReportFormatter(OldAndNewLog):
    def setUp(self):
        super(TestReportFormatter, self).setUp()
        self.tc = TestComparator(self.old_log, self.new_log, 0.05)
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
        self.assert_markdown_contains([
            'TEST                  | OLD    | NEW    | DELTA   | SPEEDUP',
            '---                   | ---    | ---    | ---     | ---    ',
            'TEST                  | MIN    | MAX    | MEAN    | MAX_RSS'])
        self.assert_git_contains([
            'TEST                    OLD      NEW      DELTA     SPEEDUP',
            'TEST                    MIN      MAX      MEAN      MAX_RSS'])
        self.assert_html_contains([
            """
                <th align='left'>OLD</th>
                <th align='left'>NEW</th>
                <th align='left'>DELTA</th>
                <th align='left'>SPEEDUP</th>""",
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


class Test_compare_perf_tests_main(OldAndNewLog):
    """Integration test that invokes the whole comparison script."""
    markdown = [
        '<summary>Regression (1)</summary>',
        'TEST                  | OLD    | NEW    | DELTA   | SPEEDUP',
        'BitCount              | 3      | 9      | +199.9% | **0.33x**',
    ]
    git = [
        'Regression (1):',
        'TEST                    OLD      NEW      DELTA     SPEEDUP',
        'BitCount                3        9        +199.9%   **0.33x**',
    ]
    html = ['<html>', "<td align='left'>BitCount</td>"]

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
