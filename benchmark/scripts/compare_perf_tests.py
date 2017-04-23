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


class PerformanceTestResult:
    def __init__(self, csv_row):
        # csv_row[0] is just an ordinal number of the test - skip that
        self.name = csv_row[1]          # Name of the performance test
        self.samples = int(csv_row[2])  # Number of measurement samples taken
        self.min = int(csv_row[3])      # Minimum runtime (ms)
        self.max = int(csv_row[4])      # Maximum runtime (ms)
        self.mean = int(csv_row[5])     # Mean (average) runtime (ms)
        self.sd = int(csv_row[6])       # Standard Deviation (ms)
        self.median = int(csv_row[7])   # Median runtime (ms)


class ResultComparison:
    def __init__(self, old, new):
        self.old = old
        self.new = new
        self.name = old.name  # Test name, convenience accessor

        # Speedup ratio FIXME no need to round here - we do that in values()
        self.ratio = round((old.min + 0.001) / (new.min + 0.001), 2)

        # Test runtime improvement in %
        ratio = (new.min + 0.001) / (old.min + 0.001)
        self.delta = round(((ratio - 1) * 100), 2)  # FIXME no need to round

        self.is_dubious = (  # FIXME this is legacy
            "(?)" if ((old.min < new.min and new.min < old.max) or
                      (new.min < old.min and old.min < new.max))
            else "")

    # Tuple of values formatted for display in results table:
    # (name, old value, new value, delta [%], speedup ratio)
    def values(self):
        return (self.name, str(self.old.min), str(self.new.min),
                "{0:+.1f}%".format(self.delta),
                "{0:.2f}x {1}".format(self.ratio, self.is_dubious))


class TestComparator:
    def __init__(self, old_file, new_file, delta_threshold, changes_only):

        def load_from_CSV(filename):
            def skip_totals(row):
                        return len(row) > 8
            tests = map(PerformanceTestResult,
                        filter(skip_totals, csv.reader(open(filename))))
            names = map(lambda t: t.name, tests)
            return dict(zip(names, tests))

        old_results = load_from_CSV(old_file)
        new_results = load_from_CSV(new_file)
        old_tests = set(old_results.keys())
        new_tests = set(new_results.keys())
        # added_tests = new_tests.difference(old_tests)
        # removed_tests = old_tests.difference(new_tests)
        comparable_tests = new_tests.intersection(old_tests)

        def compare(name):
            return ResultComparison(old_results[name], new_results[name])

        comparisons = map(compare, comparable_tests)

        def partition(l, p):
            return reduce(lambda x, y: x[not p(y)].append(y) or x, l, ([], []))

        # # When done refactoring, and about to change sort order, use this:
        # # TODO use standard deviation - SD
        # decreased, not_decreased = partition(
        #     comparisons, lambda c: c.ratio < (1 - delta_threshold))
        # increased, unchanged = partition(
        #     not_decreased, lambda c: c.ratio > (1 + delta_threshold))
        # # TODO sort decreased ascending, increased descending,
        # # unchanged alphabetically

        # To enable validation using diff during refactoring, this replicates
        # the "semi-stable" sort that due to rounding of ratio to 2 decimals
        # heavily depends on the internal implementation of dictionary. (!!!)
        ratio_map = dict(map(lambda c: (c.name, c.ratio), comparisons))
        tests_by_speedup = sorted(ratio_map.items(), key=lambda x: x[1])

        decreased, not_decreased = partition(
            tests_by_speedup, lambda x: x[1] < (1 - delta_threshold))
        increased, unchanged = partition(
            not_decreased, lambda x: x[1] > (1 + delta_threshold))
        # extra mangling for unchanged...
        u = dict(unchanged)
        unchanged = sorted(u.items(), key=lambda x: x[1], reverse=True)

        # sorted partitions
        comp_map = dict(zip(map(lambda c: c.name, comparisons), comparisons))
        self.decreased = map(lambda t: comp_map[t[0]], decreased)
        self.increased = map(lambda t: comp_map[t[0]], increased)
        self.unchanged = map(lambda t: comp_map[t[0]], unchanged)
        # TODO add alphabetically sorted lists for added, removed


class ReportFormatter:
    def __init__(self, comparator, old_branch, new_branch, changes_only):
        self.comparator = comparator
        self.old_branch = old_branch
        self.new_branch = new_branch
        self.changes_only = changes_only

    MARKDOWN_DETAIL = """
<details {3}>
  <summary>{0} ({2})</summary>
  {1}
</details>
"""
    GIT_DETAIL = """
{0}: {1}"""

    def markdown(self):
        return self.__formatted_text(
            ROW='{0} | {1} | {2} | {3} | {4} \n',
            HEADER_SEPARATOR='---',
            DETAIL=self.MARKDOWN_DETAIL)

    def git(self):
        return self.__formatted_text(
            ROW='{0}   {1}   {2}   {3}   {4} \n',
            HEADER_SEPARATOR='   ',
            DETAIL=self.GIT_DETAIL).replace('-', ' ')  # FIXME legacy replace

    def __column_widths(self, header):
        changed = self.comparator.decreased + self.comparator.increased
        comparisons = (changed if self.changes_only else
                       changed + self.comparator.unchanged)
        values = map(lambda c: c.values(), comparisons)

        def col_widths(contents):
            return map(len, contents)

        def max_widths(maximum, contents):
            return tuple(map(max, zip(maximum, col_widths(contents))))

        widths = reduce(max_widths, values, col_widths(header))
        widths = widths[:-1] + (2,)  # FIXME legacy formatting
        return widths

    def __formatted_text(self, ROW, HEADER_SEPARATOR, DETAIL):
        header = (
            'TEST', self.old_branch, self.new_branch, 'DELTA', 'SPEEDUP')

        widths = self.__column_widths(header)

        def justify_columns(contents):
            return tuple(map(lambda (w, c): c.ljust(w), zip(widths, contents)))

        def row(contents):
            return ROW.format(*justify_columns(contents))

        header = '\n' + row(header) + row(tuple([HEADER_SEPARATOR] * 5))

        def format_result(r, strong):
            # FIXME this mangling just reproduces different formatting
            # between HTML and Markdown versions of the legacy script
            r = r[:-1] + (r[-1].replace(' ', ''), )
            return (r if not strong else
                    r[:-1] + ('**{0}**'.format(r[-1]), ))

        def results(title, comp_list, is_strong=False, is_open=False):
            rows = [
                row(format_result(result_comparison.values(), is_strong))
                for result_comparison in comp_list
            ]
            return ('' if not rows else
                    DETAIL.format(*[
                        title,
                        (header + ''.join(rows)),
                        len(comp_list),
                        ('open' if is_open else '')
                    ]))

        return ''.join([
            results('Regression', self.comparator.decreased, True, True),
            results('Improvement', self.comparator.increased, True),
            results('No Changes', self.comparator.unchanged),
        ])

    HTML = """
<!DOCTYPE html>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
</head>
<body>
{0}
</body>
</html>"""

    HTML_TABLE = """
<table>
        <tr>
                <th align='left'>{0}</th>
                <th align='left'>{1}</th>
                <th align='left'>{2}</th>
                <th align='left'>{3}</th>
                <th align='left'>{4}</th>
        </tr>
        {5}
</table>
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

        def header(title):
            return row("<strong>{0}:</strong>".format(title),
                       "", "", "", "", "black")

        def results(title, comp_list, speedup_color):
            rows = [
                row(*(result_comparison.values() + (speedup_color,)))
                for result_comparison in comp_list
            ]
            return ('' if not rows else
                    header(title) + ''.join(rows))

        return self.HTML.format(self.HTML_TABLE.format(
            "TEST", self.old_branch, self.new_branch, "DELTA", "SPEEDUP",
            ''.join([
                results('Regression', self.comparator.decreased, 'red'),
                results('Improvement', self.comparator.increased, 'green'),
                ('' if self.changes_only else
                 results('No Changes', self.comparator.unchanged, 'black'))
            ])))


def main():

    parser = argparse.ArgumentParser(description="Compare Performance tests.")
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
                        help='Name of the new branch', default="NEW_MIN")
    parser.add_argument('--old-branch',
                        help='Name of the old branch', default="OLD_MIN")
    parser.add_argument('--delta-threshold',
                        help='Delta threshold. Default 0.05.', default="0.05")

    args = parser.parse_args()
    comparator = TestComparator(args.old_file, args.new_file,
                                float(args.delta_threshold), args.changes_only)
    formatter = ReportFormatter(comparator, args.old_branch, args.new_branch,
                                args.changes_only)

    if args.format:
        if args.format.lower() != "markdown":
            print(formatter.git())
        else:
            print(formatter.markdown())

    if args.format:
        if args.format.lower() == "html":
            if args.output:
                write_to_file(args.output, formatter.html())
            else:
                print("Error: missing --output flag.")
                sys.exit(1)
        elif args.format.lower() == "markdown":
            if args.output:
                write_to_file(args.output, formatter.markdown())
        elif args.format.lower() != "git":
            print("{0} is unknown format.".format(args.format))
            sys.exit(1)


def write_to_file(file_name, data):
    """
    Write data to given file
    """
    file = open(file_name, "w")
    file.write(data)
    file.close


if __name__ == "__main__":
    sys.exit(main())
