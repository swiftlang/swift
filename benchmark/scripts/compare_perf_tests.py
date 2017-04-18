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

    (old_results, old_max_results) = load_tests_CSV(args.old_file)
    (new_results, new_max_results) = load_tests_CSV(args.new_file)

    new_tests = set(new_results.keys())
    old_tests = set(old_results.keys())
    # added_tests = new_tests.difference(old_tests)
    # removed_tests = old_tests.difference(new_tests)
    comparable_tests = new_tests.intersection(old_tests)

    ratio_list = {}
    delta_list = {}
    unknown_list = {}

    for key in comparable_tests:
            ratio = (old_results[key] + 0.001) / (new_results[key] + 0.001)
            ratio_list[key] = round(ratio, 2)
            delta = (((float(new_results[key] + 0.001) /
                      (old_results[key] + 0.001)) - 1) * 100)
            delta_list[key] = round(delta, 2)
            if ((old_results[key] < new_results[key] and
                new_results[key] < old_max_results[key]) or
                (new_results[key] < old_results[key] and
                    old_results[key] < new_max_results[key])):
                    unknown_list[key] = "(?)"
            else:
                    unknown_list[key] = ""

    (complete_perf_list,
     increased_perf_list,
     decreased_perf_list,
     normal_perf_list) = sort_ratio_list(ratio_list, args.changes_only)

    """
    Create markdown formatted table
    """

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
            html_data = convert_to_html(ratio_list, old_results, new_results,
                                        delta_list, unknown_list, old_branch,
                                        new_branch, args.changes_only)

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


def load_tests_CSV(filename):
    TESTNAME = 1
    # SAMPLES = 2
    MIN = 3
    MAX = 4
    # MEAN = 5
    # SD = 6
    # MEDIAN = 7

    results = {}
    max_results = {}
    for row in csv.reader(open(filename)):
        if (len(row) > 8):  # skip Totals row
            results[row[TESTNAME]] = int(row[MIN])
            max_results[row[TESTNAME]] = int(row[MAX])
    return (results, max_results)


def convert_to_html(ratio_list, old_results, new_results, delta_list,
                    unknown_list, old_branch, new_branch, changes_only):
    (complete_perf_list,
     increased_perf_list,
     decreased_perf_list,
     normal_perf_list) = sort_ratio_list(ratio_list, changes_only)

    html_rows = ""
    for key in complete_perf_list:
        if ratio_list[key] < RATIO_MIN:
            color = "red"
        elif ratio_list[key] > RATIO_MAX:
            color = "green"
        else:
            color = "black"
        if len(decreased_perf_list) > 0 and key == decreased_perf_list[0]:
            html_rows += HTML_ROW.format(
                "<strong>Regression:</strong>",
                "", "", "", "black", "", "")
        if len(increased_perf_list) > 0 and key == increased_perf_list[0]:
            html_rows += HTML_ROW.format(
                "<strong>Improvement:</strong>",
                "", "", "", "black", "", "")
        if len(normal_perf_list) > 0 and key == normal_perf_list[0]:
            html_rows += HTML_ROW.format(
                "<strong>No Changes:</strong>",
                "", "", "", "black", "", "")

        html_rows += HTML_ROW.format(key, old_results[key],
                                     new_results[key],
                                     "{0:+.1f}%".format(delta_list[key]),
                                     color,
                                     "{0:.2f}x {1}".format(ratio_list[key],
                                                           unknown_list[key]))

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


def sort_ratio_list(ratio_list, changes_only=False):
    """
    Return 3 sorted list improvement, regression and normal.
    """
    decreased_perf_list = []
    increased_perf_list = []
    sorted_normal_perf_list = []
    normal_perf_list = {}

    for key, v in sorted(ratio_list.items(), key=lambda x: x[1]):
        if ratio_list[key] < RATIO_MIN:
            decreased_perf_list.append(key)
        elif ratio_list[key] > RATIO_MAX:
            increased_perf_list.append(key)
        else:
            normal_perf_list[key] = v

    for key, v in sorted(normal_perf_list.items(), key=lambda x: x[1],
                         reverse=True):
        sorted_normal_perf_list.append(key)

    if changes_only:
        complete_perf_list = decreased_perf_list + increased_perf_list
    else:
        complete_perf_list = (decreased_perf_list + increased_perf_list +
                              sorted_normal_perf_list)

    return (complete_perf_list, increased_perf_list,
            decreased_perf_list, sorted_normal_perf_list)


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
