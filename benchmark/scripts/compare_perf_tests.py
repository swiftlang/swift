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

import argparse
import csv
import sys

TESTNAME = 1
SAMPLES = 2
MIN = 3
MAX = 4
MEAN = 5
SD = 6
MEDIAN = 7

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

RATIO_MIN = None
RATIO_MAX = None


def main():
    global RATIO_MIN
    global RATIO_MAX

    old_results = {}
    new_results = {}
    old_max_results = {}
    new_max_results = {}
    ratio_list = {}
    delta_list = {}
    unknown_list = {}
    complete_perf_list = []
    increased_perf_list = []
    decreased_perf_list = []
    normal_perf_list = []

    parser = argparse.ArgumentParser(description="Compare Performance tests.")
    parser.add_argument('--old-file',
                        help='Baseline performance test suite (csv file)',
                        required=True)
    parser.add_argument('--new-file',
                        help='New performance test suite (csv file)',
                        required=True)
    parser.add_argument('--format',
                        help='Supported format git, html and markdown',
                        default="markdown")
    parser.add_argument('--output', help='Output file name')
    parser.add_argument('--changes-only',
                        help='Output only affected tests', action='store_true')
    parser.add_argument('--new-branch',
                        help='Name of the new branch', default="NEW_MIN")
    parser.add_argument('--old-branch',
                        help='Name of the old branch', default="OLD_MIN")
    parser.add_argument('--delta-threshold',
                        help='delta threshold', default="0.05")

    args = parser.parse_args()

    old_file = args.old_file
    new_file = args.new_file

    new_branch = args.new_branch
    old_branch = args.old_branch

    old_data = csv.reader(open(old_file))
    new_data = csv.reader(open(new_file))

    RATIO_MIN = 1 - float(args.delta_threshold)
    RATIO_MAX = 1 + float(args.delta_threshold)

    for row in old_data:
        if (len(row) > 7 and row[MIN].isdigit()):
            if row[TESTNAME] in old_results:
                if old_results[row[TESTNAME]] > int(row[MIN]):
                    old_results[row[TESTNAME]] = int(row[MIN])
                if old_max_results[row[TESTNAME]] < int(row[MAX]):
                    old_max_results[row[TESTNAME]] = int(row[MAX])
            else:
                old_results[row[TESTNAME]] = int(row[MIN])
                old_max_results[row[TESTNAME]] = int(row[MAX])

    for row in new_data:
        if (len(row) > 7 and row[MIN].isdigit()):
            if row[TESTNAME] in new_results:
                if int(new_results[row[TESTNAME]]) > int(row[MIN]):
                    new_results[row[TESTNAME]] = int(row[MIN])
                if new_max_results[row[TESTNAME]] < int(row[MAX]):
                    new_max_results[row[TESTNAME]] = int(row[MAX])
            else:
                new_results[row[TESTNAME]] = int(row[MIN])
                new_max_results[row[TESTNAME]] = int(row[MAX])

    ratio_total = 0
    for key in new_results.keys():
            ratio = (old_results[key] + 0.001) / (new_results[key] + 0.001)
            ratio_list[key] = round(ratio, 2)
            ratio_total *= ratio
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
    test_name_width = max_width(ratio_list, title='TEST', key_len=True)
    new_time_width = max_width(new_results, title=new_branch)
    old_time_width = max_width(old_results, title=old_branch)
    delta_width = max_width(delta_list, title='DELTA (%)')

    markdown_table_header = "\n" + MARKDOWN_ROW.format(
        "TEST".ljust(test_name_width),
        old_branch.ljust(old_time_width),
        new_branch.ljust(new_time_width),
        "DELTA (%)".ljust(delta_width),
        "SPEEDUP".ljust(2))
    markdown_table_header += MARKDOWN_ROW.format(
        HEADER_SPLIT.ljust(test_name_width),
        HEADER_SPLIT.ljust(old_time_width),
        HEADER_SPLIT.ljust(new_time_width),
        HEADER_SPLIT.ljust(delta_width),
        HEADER_SPLIT.ljust(2))
    markdown_regression = ""
    for i, key in enumerate(decreased_perf_list):
        ratio = "{0:.2f}x".format(ratio_list[key])
        if i == 0:
            markdown_regression = markdown_table_header
        markdown_regression += MARKDOWN_ROW.format(
            key.ljust(test_name_width),
            str(old_results[key]).ljust(old_time_width),
            str(new_results[key]).ljust(new_time_width),
            ("{0:+.1f}%".format(delta_list[key])).ljust(delta_width),
            "**{0}{1}**".format(str(ratio).ljust(2), unknown_list[key]))

    markdown_improvement = ""
    for i, key in enumerate(increased_perf_list):
        ratio = "{0:.2f}x".format(ratio_list[key])
        if i == 0:
            markdown_improvement = markdown_table_header
        markdown_improvement += MARKDOWN_ROW.format(
            key.ljust(test_name_width),
            str(old_results[key]).ljust(old_time_width),
            str(new_results[key]).ljust(new_time_width),
            ("{0:+.1f}%".format(delta_list[key])).ljust(delta_width),
            "**{0}{1}**".format(str(ratio).ljust(2), unknown_list[key]))

    markdown_normal = ""
    for i, key in enumerate(normal_perf_list):
        ratio = "{0:.2f}x".format(ratio_list[key])
        if i == 0:
            markdown_normal = markdown_table_header
        markdown_normal += MARKDOWN_ROW.format(
            key.ljust(test_name_width),
            str(old_results[key]).ljust(old_time_width),
            str(new_results[key]).ljust(new_time_width),
            ("{0:+.1f}%".format(delta_list[key])).ljust(delta_width),
            "{0}{1}".format(str(ratio).ljust(2), unknown_list[key]))

    markdown_data = MARKDOWN_DETAIL.format("Regression",
                                           len(decreased_perf_list),
                                           markdown_regression, "open")
    markdown_data += MARKDOWN_DETAIL.format("Improvement",
                                            len(increased_perf_list),
                                            markdown_improvement, "")
    if not args.changes_only:
        markdown_data += MARKDOWN_DETAIL.format("No Changes",
                                                len(normal_perf_list),
                                                markdown_normal, "")

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
                                   "DELTA (%)", "SPEEDUP", html_rows)
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


def max_width(items, title, key_len=False):
    """
    Returns the max length of string in the list
    """
    width = len(str(title))
    for key in items.keys():
        if key_len:
            if width < len(str(key)):
                width = len(str(key))
        else:
            if width < len(str(items[key])):
                width = len(str(items[key]))
    return width


if __name__ == "__main__":
        sys.exit(main())
