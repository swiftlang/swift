#!/usr/bin/python
# -*- coding: utf-8 -*-

# ===--- compare_perf_tests.py --------------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See http://swift.org/LICENSE.txt for license information
#  See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===//

# e.g.
# repeat.sh 3 tot/bin/Benchmark_Driver run -o -O > tot.O.times
# repeat.sh 3 mypatch/bin/Benchmark_Driver run -o -O > mypatch.O.times
# compare_perf_tests.py tot.O.times mypatch.O.times | sort -t, -n -k 6 | \
#     column -s, -t

from __future__ import print_function

import re
import sys


VERBOSE = 0

# #,TEST,SAMPLES,MIN(ms),MAX(ms),MEAN(ms),SD(ms),MEDIAN(ms)
SCORERE = re.compile(
    r"(\d+),[ \t]*(\w+),[ \t]*([\d.]+),[ \t]*([\d.]+),[ \t]*([\d.]+)")
TOTALRE = re.compile(
    r"()(Totals),[ \t]*([\d.]+),[ \t]*([\d.]+),[ \t]*([\d.]+)")
NUMGROUP = 1
KEYGROUP = 2
BESTGROUP = 4
WORSTGROUP = 5

IsTime = 1
ShowSpeedup = 1
PrintAllScores = 0


def parse_int(word):
    try:
        return int(word)
    except ValueError:
        raise Exception("Expected integer value, not " + word)


def get_scores(fname):
    scores = {}
    worstscores = {}
    nums = {}
    runs = 0
    f = open(fname)
    try:
        for line in f:
            if VERBOSE:
                print("Parsing", line, end="")
            m = SCORERE.match(line)
            is_total = False
            if not m:
                is_total = True
                m = TOTALRE.match(line)
            if not m:
                continue

            if VERBOSE:
                print("  match", m.group(KEYGROUP), m.group(BESTGROUP))

            if not m.group(KEYGROUP) in scores:
                scores[m.group(KEYGROUP)] = []
                worstscores[m.group(KEYGROUP)] = []
            scores[m.group(KEYGROUP)].append(parse_int(m.group(BESTGROUP)))
            worstscores[m.group(KEYGROUP)].append(
                parse_int(m.group(WORSTGROUP)))
            if is_total:
                nums[m.group(KEYGROUP)] = ""
            else:
                nums[m.group(KEYGROUP)] = m.group(NUMGROUP)
            if len(scores[m.group(KEYGROUP)]) > runs:
                runs = len(scores[m.group(KEYGROUP)])
    finally:
        f.close()
    return scores, worstscores, runs, nums


def is_max_score(newscore, maxscore, invert):
    return not maxscore or \
        (newscore > maxscore if not invert else newscore < maxscore)


def compare_scores(key, score1, worstsample1, score2, worstsample2, runs, num):
    print(num.rjust(3), end="")
    print(key.ljust(25), end="")
    bestscore1 = None
    bestscore2 = None
    worstscore1 = None
    worstscore2 = None
    minbest = IsTime
    minworst = not minbest
    r = 0
    for score in score1:
        if is_max_score(newscore=score, maxscore=bestscore1, invert=minbest):
            bestscore1 = score
        if is_max_score(newscore=score, maxscore=worstscore1, invert=minworst):
            worstscore1 = score
        if PrintAllScores:
            print (("%d" % score).rjust(16), end="")
    for score in worstsample1:
        if is_max_score(newscore=score, maxscore=worstscore1, invert=minworst):
            worstscore1 = score
    for score in score2:
        if is_max_score(newscore=score, maxscore=bestscore2, invert=minbest):
            bestscore2 = score
        if is_max_score(newscore=score, maxscore=worstscore2, invert=minworst):
            worstscore2 = score
        if PrintAllScores:
            print (("%d" % score).rjust(16), end="")
        r += 1
    for score in worstsample2:
        if is_max_score(newscore=score, maxscore=worstscore2, invert=minworst):
            worstscore2 = score
    while r < runs:
        if PrintAllScores:
            print ("0".rjust(9), end="")
        r += 1

    if not PrintAllScores:
        print (("%d" % bestscore1).rjust(16), end="")
        print (("%d" % bestscore2).rjust(16), end="")

    print (("%+d" % (bestscore2 - bestscore1)).rjust(9), end="")

    if bestscore1 != 0 and bestscore2 != 0:
        print(("%+.1f%%" %
               (((float(bestscore2) / bestscore1) - 1) * 100)).rjust(9), end="")
        if ShowSpeedup:
            Num, Den = float(bestscore2), float(bestscore1)
            if IsTime:
                Num, Den = Den, Num
            print (("%.2fx" % (Num / Den)).rjust(9), end="")
    else:
        print("*".rjust(9), end="")
        if ShowSpeedup:
            print("*".rjust(9), end="")
    # check if the worst->best interval for each configuration overlap.
    if minbest:
        if (bestscore1 < bestscore2 and bestscore2 < worstscore1) \
           or (bestscore2 < bestscore1 and bestscore1 < worstscore2):
            print("(?)", end="")
    else:
        if (worstscore1 < worstscore2 and worstscore2 < bestscore1) \
           or (worstscore2 < worstscore1 and worstscore1 < bestscore2):
            print("(?)", end="")
    print()


def print_best_scores(key, scores):
    print(key, end="")
    bestscore = None
    minbest = IsTime
    for score in scores:
        if is_max_score(newscore=score, maxscore=bestscore, invert=minbest):
            bestscore = score
    print(", %d" % bestscore)


def usage():
    print("repeat.sh <n> Benchmark_O[none|unchecked] > file.times")
    print("compare_perf_tests.py <file.times> [<file2.times>]")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        usage()
        sys.exit(1)
    file1 = sys.argv[1]
    if len(sys.argv) < 3:
        scores, worstscores, runs, nums = get_scores(file1)
        keys = list(set(scores.keys()))
        keys.sort()
        for key in keys:
            print_best_scores(key, scores[key])
        sys.exit(0)

    file2 = sys.argv[2]
    if len(sys.argv) > 3:
        SCORERE = re.compile(sys.argv[3])

    scores1, worstscores1, runs1, nums = get_scores(file1)
    scores2, worstscores2, runs2, nums = get_scores(file2)

    runs = runs1
    if runs2 > runs:
        runs = runs2

    if VERBOSE:
        print(scores1)
        print(scores2)

    keys = list(set(scores1.keys() + scores2.keys()))
    keys.sort()
    if VERBOSE:
        print("comparing ", file1, "vs", file2, "=", end="")
        if IsTime:
            print(file1, "/", file2)
        else:
            print(file2, "/", file1)

    print("#".rjust(3), end="")
    print("TEST".ljust(25), end="")
    if PrintAllScores:
        for i in range(0, runs):
            print(("OLD_RUN%d" % i).rjust(9), end="")
        for i in range(0, runs):
            print(("NEW_RUN%d" % i).rjust(9), end="")
    else:
        print("BEST_OLD_MIN(μs)".rjust(17), end="")
        print("BEST_NEW_MIN(μs)".rjust(17), end="")
    print('DELTA'.rjust(9), '%DELTA'.rjust(9), 'SPEEDUP'.rjust(9))

    for key in keys:
        if key not in scores1:
            print(key, "not in", file1)
            continue
        if key not in scores2:
            print(key, "not in", file2)
            continue
        compare_scores(key, scores1[key], worstscores1[key], scores2[key],
                       worstscores2[key], runs, nums[key])
