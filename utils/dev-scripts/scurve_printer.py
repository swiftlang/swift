#!/usr/bin/env python

# This is a simple script that takes in an scurve file produced by
# csvcolumn_to_scurve and produces a png graph of the scurve.

import argparse
import csv

import matplotlib.pyplot as plt

import numpy as np

FIELDS = ['N/total', 'New/Old']


def get_data(input_file):
    global FIELDS
    for row in csv.DictReader(input_file):
        yield (float(row[FIELDS[0]]), float(row[FIELDS[1]]))


def main():
    p = argparse.ArgumentParser()
    p.add_argument('input_csv_file', type=argparse.FileType('r'))
    p.add_argument('output_file', type=str)
    p.add_argument('-y-axis-num-tick-marks', type=int,
                   help='The number of y tick marks to use above/below zero.')
    p.add_argument('-y-axis-min', type=float,
                   help='Override the min y axis that we use')
    p.add_argument('-y-axis-max', type=float,
                   help='Override the min y axis that we use')
    p.add_argument('-title', type=str,
                   help='Title of the graph')
    p.add_argument('-x-axis-title', type=str,
                   help='The title to use on the x-axis of the graph')
    p.add_argument('-y-axis-title', type=str,
                   help='The title to use on the x-axis of the graph')

    args = p.parse_args()

    data = np.array(list(get_data(args.input_csv_file)))
    assert np.all(data >= 0)

    x = data[:, 0]
    y = data[:, 1]

    x_axis_title = args.x_axis_title or FIELDS[0]
    y_axis_title = args.y_axis_title or FIELDS[1]
    title = args.title or "{} vs {}".format(x_axis_title, y_axis_title)

    fig, ax = plt.subplots()
    fig.set_size_inches(18.5, 18.5)

    fig.suptitle(title, fontsize=20)
    ax.set_xlabel(x_axis_title, fontsize=20)
    ax.set_ylabel(y_axis_title, fontsize=20)
    ax.plot(x, y)
    ax.scatter(x, y)

    # To get good bounds, we:
    #
    # 1. Re-center our data at 0 by subtracting 1. This will give us the %
    # difference in between new and old (i.e. (new - old)/old)
    #
    # 2. Then we take the maximum absolute delta from zero and round to a
    # multiple of 5 away from zero. Lets call this value limit.
    #
    # 3. We set [min_y, max_y] = [1.0 - limit, 1.0 + limit]
    recentered_data = y - 1.0
    max_magnitude = int(np.max(np.abs(recentered_data)) * 100.0)
    y_limit = float(((max_magnitude // 5) + 1) * 5) * 0.01

    ax.set_xlim(0.0, 1.0)
    y_min = args.y_axis_min or 1.0 - y_limit
    y_max = args.y_axis_max or 1.0 + y_limit
    assert(y_min <= y_max)
    ax.set_ylim(y_min, y_max)
    ax.grid(True)
    ax.xaxis.set_ticks(np.arange(0.0, 1.0, 0.05))
    if args.y_axis_num_tick_marks:
        y_delta = y_max - y_min
        y_tickmark_frequency = y_delta / float(args.y_axis_num_tick_marks)
        ax.yaxis.set_ticks(np.arange(y_min, y_max, y_tickmark_frequency))
    plt.savefig(args.output_file)


if __name__ == "__main__":
    main()
