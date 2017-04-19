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

        # Speedup ratio
        self.ratio = round((old.min + 0.001) / (new.min + 0.001), 2)

        # Test runtime improvement in %
        ratio = (new.min + 0.001) / (old.min + 0.001)
        self.delta = round(((ratio - 1) * 100), 2)

        self.is_dubious = (
            "(?)" if ((old.min < new.min and new.min < old.max) or
                      (new.min < old.min and old.min < new.max))
            else "")


class TestComparator:
    def __init__(self, old_file, new_file, delta_threshold, changes_only):
        self.delta_threshold = float(delta_threshold)  # TODO remove
        self.changes_only = changes_only

        def load_from_CSV(filename):
            def skip_totals(row):
                        return len(row) > 8
            tests = map(PerformanceTestResult,
                        filter(skip_totals, csv.reader(open(filename))))
            names = map(lambda t: t.name, tests)
            return dict(zip(names, tests))

        self.old_results = load_from_CSV(old_file)
        self.new_results = load_from_CSV(new_file)
        self.old_tests = set(self.old_results.keys())
        self.new_tests = set(self.new_results.keys())
        self.added_tests = self.new_tests.difference(self.old_tests)
        self.removed_tests = self.old_tests.difference(self.new_tests)
        self.comparable_tests = self.new_tests.intersection(self.old_tests)

        def extract(property, list):
            return lambda name: (name, getattr(list[name], property))

        old, new = [sorted(list(s)) for s in [self.old_tests, self.new_tests]]
        self.old_min_results = dict(map(extract("min", self.old_results), old))
        self.old_max_results = dict(map(extract("max", self.old_results), old))
        self.new_min_results = dict(map(extract("min", self.new_results), new))
        self.new_max_results = dict(map(extract("max", self.new_results), new))

        def compare(property):
            return lambda name: (name, getattr(self.compare(name), property))

        self.ratio_list = dict(map(compare("ratio"), self.comparable_tests))
        self.delta_list = dict(map(compare("delta"), self.comparable_tests))
        self.unknown_list = dict(map(compare("is_dubious"),
                                 self.comparable_tests))

        def has_decreased((_, speedup_ratio)):
            return speedup_ratio < (1 - delta_threshold)

        def has_increased((_, speedup_ratio)):
            return speedup_ratio > (1 + delta_threshold)

        def partition(l, p):
            return reduce(lambda x, y: x[not p(y)].append(y) or x, l, ([], []))

        tests_by_speedup = sorted(self.ratio_list.items(), key=lambda x: x[1])
        decreased, not_decreased = partition(tests_by_speedup, has_decreased)
        increased, unchanged = partition(not_decreased, has_increased)

        def get_name((name, _)):
            return name

        self.decreased_perf_list = map(get_name, decreased)
        self.increased_perf_list = map(get_name, increased)

        # following double sorting is required to replicate same sort order as
        # orignal script, for purposes of validation via diff, while refactoring
        unchanged = dict(unchanged)
        unchanged = sorted(unchanged.items(), key=lambda x: x[1], reverse=True)

        self.normal_perf_list = map(get_name, unchanged)

        changes = self.decreased_perf_list + self.increased_perf_list

        self.complete_perf_list = (changes if self.changes_only else
                                   changes + self.normal_perf_list)


    def compare(self, name):
        return ResultComparison(self.old_results[name], self.new_results[name])


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

    new_branch = args.new_branch
    old_branch = args.old_branch

    global RATIO_MIN
    global RATIO_MAX
    RATIO_MIN = 1 - float(args.delta_threshold)
    RATIO_MAX = 1 + float(args.delta_threshold)

    comparator = TestComparator(args.old_file, args.new_file,
                                float(args.delta_threshold), args.changes_only)

    old_results = comparator.old_min_results
    old_max_results = comparator.old_max_results
    new_results = comparator.new_min_results
    new_max_results = comparator.new_max_results

    ratio_list = comparator.ratio_list
    delta_list = comparator.delta_list
    unknown_list = comparator.unknown_list

    decreased_perf_list = comparator.decreased_perf_list
    increased_perf_list = comparator.increased_perf_list
    normal_perf_list = comparator.normal_perf_list
    complete_perf_list = comparator.complete_perf_list

    (markdown_regression,
     markdown_improvement,
     markdown_normal) = convert_to_markdown(comparator,
                                            old_branch, new_branch,
                                            args.changes_only)

    markdown_data = MARKDOWN_DETAIL.format(
        "Regression", len(decreased_perf_list), markdown_regression, "open")
    markdown_data += MARKDOWN_DETAIL.format(
        "Improvement", len(increased_perf_list), markdown_improvement, "")
    if not args.changes_only:
        markdown_data += MARKDOWN_DETAIL.format(
            "No Changes", len(normal_perf_list), markdown_normal, "")

    if args.format:
        if args.format.lower() != "markdown":
            pain_data = PAIN_DETAIL.format("Regression", markdown_regression)
            pain_data += PAIN_DETAIL.format("Improvement",
                                            markdown_improvement)
            if not args.changes_only:
                pain_data += PAIN_DETAIL.format("No Changes", markdown_normal)

            print(pain_data.replace("|", " ").replace("-", " "))
        else:
            print(markdown_data)

    if args.format:
        if args.format.lower() == "html":
            """
            Create HTML formatted table
            """
            html_data = convert_to_html(comparator, old_branch, new_branch,
                                        args.changes_only)

            if args.output:
                write_to_file(args.output, html_data)
            else:
                print("Error: missing --output flag.")
                sys.exit(1)
        elif args.format.lower() == "markdown":
            if args.output:
                write_to_file(args.output, markdown_data)
        elif args.format.lower() != "git":
            print("{0} is unknown format.".format(args.format))
            sys.exit(1)


def convert_to_markdown(comparator, old_branch, new_branch, changes_only):
    """
    Create markdown formatted table
    """
    old_results = comparator.old_min_results
    new_results = comparator.new_min_results

    ratio_list = comparator.ratio_list
    delta_list = comparator.delta_list
    unknown_list = comparator.unknown_list

    decreased_perf_list = comparator.decreased_perf_list
    increased_perf_list = comparator.increased_perf_list
    normal_perf_list = comparator.normal_perf_list
    complete_perf_list = comparator.complete_perf_list

    def max_width(items, title, key_len=False):
        def length(key):
            return len(str(key)) if key_len else len(str(items[key]))
        return max(len(title), max(map(length, items.keys())))

    widths = (  # column widths
        max_width(ratio_list, 'TEST', key_len=True),
        max_width(new_results, str(new_branch)),
        max_width(old_results, str(old_branch)),
        max_width(delta_list, 'DELTA (%)'),
        2
    )

    def justify_columns(contents):
        return tuple(map(lambda (w, c): c.ljust(w), zip(widths, contents)))

    def add_row(contents):
        return MARKDOWN_ROW.format(* justify_columns(contents))

    header = ("TEST", old_branch, new_branch, "DELTA", "SPEEDUP")
    markdown_table_header = "\n" + add_row(header)
    markdown_table_header += add_row(tuple([HEADER_SPLIT] * len(header)))

    def markdown_table(perf_list, strong):
        markdown_table = markdown_table_header
        for key in perf_list:
            ratio = "{0:.2f}x".format(ratio_list[key])
            markdown_table += add_row(
                (
                    key, str(old_results[key]), str(new_results[key]),
                    "{0:+.1f}%".format(delta_list[key]),
                    ("**{0}{1}**" if strong else "{0}{1}")
                    .format(str(ratio), unknown_list[key])
                )
            )
        return markdown_table

    markdown_regression = markdown_table(decreased_perf_list, True)
    markdown_improvement = markdown_table(increased_perf_list, True)
    markdown_normal = markdown_table(normal_perf_list, False)

    return (markdown_regression, markdown_improvement, markdown_normal)


def convert_to_html(comparator, old_branch, new_branch, changes_only):
    old_results = comparator.old_min_results
    new_results = comparator.new_min_results

    ratio_list = comparator.ratio_list
    delta_list = comparator.delta_list
    unknown_list = comparator.unknown_list

    decreased_perf_list = comparator.decreased_perf_list
    increased_perf_list = comparator.increased_perf_list
    normal_perf_list = comparator.normal_perf_list
    complete_perf_list = comparator.complete_perf_list

    def add_row(name, old, new, delta, speedup, speedup_color):
        return HTML_ROW.format(name, old, new, delta, speedup_color, speedup)

    def separator_header(title):
        return add_row("<strong>{0}:</strong>".format(title),
                       "", "", "", "", "black")

    def results_table(title, list, speedup_color):
        html_rows = separator_header(title)
        for key in list:
            html_rows += add_row(key, old_results[key], new_results[key],
                                 "{0:+.1f}%".format(delta_list[key]),
                                 "{0:.2f}x {1}".format(ratio_list[key],
                                                       unknown_list[key]),
                                 speedup_color)
        return html_rows

    html_rows = ""
    if len(decreased_perf_list) > 0:
        html_rows += results_table('Regression', decreased_perf_list, 'red')
    if len(increased_perf_list) > 0:
        html_rows += results_table('Improvement', increased_perf_list, 'green')
    if len(normal_perf_list) > 0:
        html_rows += results_table('No Changes', normal_perf_list, 'black')

    html_table = HTML_TABLE.format("TEST", old_branch, new_branch,
                                   "DELTA", "SPEEDUP", html_rows)
    html_data = HTML.format(html_table)
    return html_data


def write_to_file(file_name, data):
    """
    Write data to given file
    """
    file = open(file_name, "w")
    file.write(data)
    file.close


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

MARKDOWN_ROW = "{0} | {1} | {2} | {3} | {4} \n"
HEADER_SPLIT = "---"
MARKDOWN_DETAIL = """
<details {3}>
  <summary>{0} ({1})</summary>
  {2}
</details>
"""

PAIN_DETAIL = """
{0}: {1}"""

if __name__ == "__main__":
    sys.exit(main())
