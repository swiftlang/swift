#!/usr/bin/env python

# This is a simple script that reads in a csv file, selects a column, and then
# forms an "s-curve" graph of that column.

import argparse
import csv
import sys


def get_data(input_file, before_column, after_column):

    def get_selected_csv_rows(input_file, before_column, after_column):
        for row in csv.DictReader(input_file):
            before = float(row[before_column])
            after = float(row[after_column])
            if before > 0:
                delta = after / before
            else:
                delta = 1
            yield delta

    def f(input_data):
        result = list(enumerate(sorted(input_data)))
        count = float(len(result) - 1)
        return [(x[0] / count, x[1]) for x in result]

    return f(get_selected_csv_rows(input_file, before_column, after_column))


def main():
    p = argparse.ArgumentParser(description="""

    A script that reads in a csv file, splices out selected before/after
    column, and then outputs a new csv file with that data in s-curve form. An
    s-curve is a graph where one sorts the output %-change and graphs the %-n
    vs %-change.

    NOTE: We assume that the csv has a csv header that maps to the before and
    after column names passed in.
    """)

    p.add_argument('input_file', type=argparse.FileType('r'))
    p.add_argument('before_column_name', type=str)
    p.add_argument('after_column_name', type=str)

    args = p.parse_args()

    data = get_data(args.input_file, args.before_column_name,
                    args.after_column_name)
    w = csv.DictWriter(sys.stdout, fieldnames=['N/total', 'New/Old'])
    w.writeheader()
    for d in data:
        w.writerow({'N/total': d[0], 'New/Old': d[1]})


if __name__ == "__main__":
    main()
