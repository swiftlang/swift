#!/usr/bin/python
# -*- coding: utf-8 -*-

# ===--- compare_perf_tests.py -------------------------------------------===//
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

from __future__ import print_function

import argparse
import csv
import sys
from math import sqrt


class PerformanceTestResult:
    def __init__(self, csv_row):
        # csv_row[0] is just an ordinal number of the test - skip that
        self.name = csv_row[1]          # Name of the performance test
        self.samples = int(csv_row[2])  # Number of measurement samples taken
        self.min = int(csv_row[3])      # Minimum runtime (ms)
        self.max = int(csv_row[4])      # Maximum runtime (ms)
        self.mean = int(csv_row[5])     # Mean (average) runtime (ms)
        sd = int(csv_row[6])            # Standard Deviation (ms)
        # For computing running variance
        self.S_runtime = (0 if self.samples < 2 else
                          (sd * sd) * (self.samples - 1))
        self.median = int(csv_row[7])   # Median runtime (ms)
        self.max_rss = (                # Maximum Resident Set Size (B)
            int(csv_row[8]) if len(csv_row) > 8 else None)
        # TODO if we really want to compute mean MAX_RSS: self.S_memory

    @property
    def sd(self):  # Standard Deviation (ms)
        return (0 if self.samples < 2 else
                sqrt(self.S_runtime / (self.samples - 1)))

    # Compute running variance, B. P. Welford's method
    # See Knuth TAOCP vol 2, 3rd edition, page 232, or
    # https://www.johndcook.com/blog/standard_deviation/
    # M is mean, Standard Deviation is defined as sqrt(S/k-1)
    @staticmethod
    def running_mean_variance((k, M_, S_), x):
        k = float(k + 1)
        M = M_ + (x - M_) / k
        S = S_ + (x - M_) * (x - M)
        return (k, M, S)

    def merge(self, r):
        self.min = min(self.min, r.min)
        self.max = max(self.max, r.max)
        # self.median = None # unclear what to do here

        def push(x):
            state = (self.samples, self.mean, self.S_runtime)
            state = self.running_mean_variance(state, x)
            (self.samples, self.mean, self.S_runtime) = state

        # Merging test results with up to 3 samples is exact
        # TODO investigate how to best handle merge of higher sample counts
        values = [r.min, r.max, r.median, r.mean][:min(r.samples, 4)]
        map(push, values)

    header = ('TEST', 'MIN', 'MAX', 'MEAN', 'MAX_RSS')

    # Tuple of values formatted for display in results table:
    # (name, min value, max value, mean value, max_rss)
    def values(self):
        return (self.name, str(self.min), str(self.max), str(int(self.mean)),
                str(self.max_rss) if self.max_rss else '-')


class ResultComparison:
    def __init__(self, old, new):
        self.old = old
        self.new = new
        self.name = old.name  # Test name, convenience accessor

        # Speedup ratio
        self.ratio = (old.min + 0.001) / (new.min + 0.001)

        # Test runtime improvement in %
        ratio = (new.min + 0.001) / (old.min + 0.001)
        self.delta = ((ratio - 1) * 100)

        self.is_dubious = (  # FIXME this is legacy
            ' (?)' if ((old.min < new.min and new.min < old.max) or
                       (new.min < old.min and old.min < new.max))
            else '')

    header = ('TEST', 'OLD', 'NEW', 'DELTA', 'SPEEDUP')

    # Tuple of values formatted for display in results table:
    # (name, old value, new value, delta [%], speedup ratio)
    def values(self):
        return (self.name, str(self.old.min), str(self.new.min),
                '{0:+.1f}%'.format(self.delta),
                '{0:.2f}x{1}'.format(self.ratio, self.is_dubious))


class TestComparator:
    def __init__(self, old_file, new_file, delta_threshold, changes_only):

        def load_from_CSV(filename):  # handles output from Benchmark_O and
            def skip_totals(row):     # Benchmark_Driver (added MAX_RSS column)
                        return len(row) > 7 and row[0].isdigit()
            tests = map(PerformanceTestResult,
                        filter(skip_totals, csv.reader(open(filename))))

            def add_or_merge(names, r):
                if r.name not in names:
                    names[r.name] = r
                else:
                    names[r.name].merge(r)
                return names
            return reduce(add_or_merge, tests, dict())

        old_results = load_from_CSV(old_file)
        new_results = load_from_CSV(new_file)
        old_tests = set(old_results.keys())
        new_tests = set(new_results.keys())
        comparable_tests = new_tests.intersection(old_tests)
        added_tests = new_tests.difference(old_tests)
        removed_tests = old_tests.difference(new_tests)

        self.added = sorted(map(lambda t: new_results[t], added_tests),
                            key=lambda r: r.name)
        self.removed = sorted(map(lambda t: old_results[t], removed_tests),
                              key=lambda r: r.name)

        def compare(name):
            return ResultComparison(old_results[name], new_results[name])

        comparisons = map(compare, comparable_tests)

        def partition(l, p):
            return reduce(lambda x, y: x[not p(y)].append(y) or x, l, ([], []))

        # TODO take standard deviation (SD) into account
        decreased, not_decreased = partition(
            comparisons, lambda c: c.ratio < (1 - delta_threshold))
        increased, unchanged = partition(
            not_decreased, lambda c: c.ratio > (1 + delta_threshold))

        # sorted partitions
        names = map(lambda c: c.name, comparisons)
        comparisons = dict(zip(names, comparisons))
        self.decreased = map(lambda c: comparisons[c.name],
                             sorted(decreased, key=lambda c: -c.delta))
        self.increased = map(lambda c: comparisons[c.name],
                             sorted(increased, key=lambda c: c.delta))
        self.unchanged = map(lambda c: comparisons[c.name],
                             sorted(unchanged, key=lambda c: c.name))


class ReportFormatter:
    def __init__(self, comparator, old_branch, new_branch, changes_only):
        self.comparator = comparator
        self.old_branch = old_branch
        self.new_branch = new_branch
        self.changes_only = changes_only

    MARKDOWN_DETAIL = """
<details {3}>
  <summary>{0} ({1})</summary>
  {2}
</details>
"""
    GIT_DETAIL = """
{0} ({1}): {2}"""

    def markdown(self):
        return self.__formatted_text(
            ROW='{0} | {1} | {2} | {3} | {4} \n',
            HEADER_SEPARATOR='---',
            DETAIL=self.MARKDOWN_DETAIL)

    def git(self):
        return self.__formatted_text(
            ROW='{0}   {1}   {2}   {3}   {4} \n',
            HEADER_SEPARATOR='   ',
            DETAIL=self.GIT_DETAIL)

    def __column_widths(self):
        changed = self.comparator.decreased + self.comparator.increased
        comparisons = (changed if self.changes_only else
                       changed + self.comparator.unchanged)
        comparisons += self.comparator.added + self.comparator.removed

        values = map(lambda c: c.values(), comparisons)
        widths = map(lambda columns: map(len, columns),
                     [PerformanceTestResult.header, ResultComparison.header] +
                     values)

        def max_widths(maximum, widths):
            return tuple(map(max, zip(maximum, widths)))

        return reduce(max_widths, widths, tuple([0] * 5))

    def __formatted_text(self, ROW, HEADER_SEPARATOR, DETAIL):
        widths = self.__column_widths()

        def justify_columns(contents):
            return tuple(map(lambda (w, c): c.ljust(w), zip(widths, contents)))

        def row(contents):
            return ROW.format(*justify_columns(contents))

        def header(header):
            return '\n' + row(header) + row(tuple([HEADER_SEPARATOR] * 5))

        def format_columns(r, strong):
            return (r if not strong else
                    r[:-1] + ('**{0}**'.format(r[-1]), ))

        def table(title, results, is_strong=False, is_open=False):
            rows = [
                row(format_columns(result_comparison.values(), is_strong))
                for result_comparison in results
            ]
            return ('' if not rows else
                    DETAIL.format(*[
                        title, len(results),
                        (header(results[0].header) + ''.join(rows)),
                        ('open' if is_open else '')
                    ]))

        return ''.join([
            # FIXME print self.old_branch, self.new_branch
            table('Regression', self.comparator.decreased, True, True),
            table('Improvement', self.comparator.increased, True),
            ('' if self.changes_only else
             table('No Changes', self.comparator.unchanged)),
            table('Added', self.comparator.added, is_open=True),
            table('Removed', self.comparator.removed, is_open=True)
        ])

    HTML = """
<!DOCTYPE html>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <style>
        body {{ font-family: -apple-system, sans-serif; font-size: 14px; }}
        table {{ border-spacing: 2px; border-color: gray; border-spacing: 0;
                border-collapse: collapse; }}
        table tr {{ background-color: #fff; border-top: 1px solid #c6cbd1; }}
        table th, table td {{ padding: 6px 13px; border: 1px solid #dfe2e5; }}
        th {{ text-align: center; padding-top: 130px; }}
        td {{ text-align: right; }}
        table td:first-child {{ text-align: left; }}
        tr:nth-child(even) {{ background-color: #000000; }}
        tr:nth-child(2n) {{ background-color: #f6f8fa; }}
    </style>
</head>
<body>
<table>
{0}
</table>
</body>
</html>"""

    HTML_HEADER_ROW = """
        <tr>
                <th align='left'>{0} ({1})</th>
                <th align='left'>{2}</th>
                <th align='left'>{3}</th>
                <th align='left'>{4}</th>
                <th align='left'>{5}</th>
        </tr>
"""

    HTML_ROW = """
        <tr>
                <td align='left'>{0}</td>
                <td align='left'>{1}</td>
                <td align='left'>{2}</td>
                <td align='left'>{3}</td>
                <td align='left'><font color='{4}'>{5}</font></td>
        </tr>
"""

    def html(self):

        def row(name, old, new, delta, speedup, speedup_color):
            return self.HTML_ROW.format(
                name, old, new, delta, speedup_color, speedup)

        def header(contents):
            return self.HTML_HEADER_ROW.format(* contents)

        def table(title, results, speedup_color):
            rows = [
                row(*(result_comparison.values() + (speedup_color,)))
                for result_comparison in results
            ]
            return ('' if not rows else
                    header((title, len(results)) + results[0].header[1:]) +
                    ''.join(rows))

        return self.HTML.format(
            ''.join([
                # FIXME print self.old_branch, self.new_branch
                table('Regression', self.comparator.decreased, 'red'),
                table('Improvement', self.comparator.increased, 'green'),
                ('' if self.changes_only else
                 table('No Changes', self.comparator.unchanged, 'black')),
                table('Added', self.comparator.added, ''),
                table('Removed', self.comparator.removed, '')
            ]))


def main():

    parser = argparse.ArgumentParser(description='Compare Performance tests.')
    parser.add_argument('--old-file',
                        help='Baseline performance test suite (csv file)',
                        required=True)
    parser.add_argument('--new-file',
                        help='New performance test suite (csv file)',
                        required=True)
    parser.add_argument('--format',
                        choices=['markdown', 'git', 'html'],
                        help='Output format. Default is markdown.',
                        default="markdown")
    parser.add_argument('--output', help='Output file name')
    parser.add_argument('--changes-only',
                        help='Output only affected tests', action='store_true')
    parser.add_argument('--new-branch',
                        help='Name of the new branch', default='NEW_MIN')
    parser.add_argument('--old-branch',
                        help='Name of the old branch', default='OLD_MIN')
    parser.add_argument('--delta-threshold',
                        help='Delta threshold. Default 0.05.', default='0.05')

    args = parser.parse_args()
    comparator = TestComparator(args.old_file, args.new_file,
                                float(args.delta_threshold), args.changes_only)
    formatter = ReportFormatter(comparator, args.old_branch, args.new_branch,
                                args.changes_only)

    if args.format:
        if args.format.lower() != 'markdown':
            print(formatter.git())
        else:
            print(formatter.markdown())

    if args.format:
        if args.format.lower() == 'html':
            if args.output:
                write_to_file(args.output, formatter.html())
            else:
                print('Error: missing --output flag.')
                sys.exit(1)
        elif args.format.lower() == 'markdown':
            if args.output:
                write_to_file(args.output, formatter.markdown())
        elif args.format.lower() != 'git':
            print('{0} is unknown format.'.format(args.format))
            sys.exit(1)


def write_to_file(file_name, data):
    """
    Write data to given file
    """
    file = open(file_name, 'w')
    file.write(data)
    file.close


if __name__ == '__main__':
    sys.exit(main())
