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
# compare_perf_tests.py tot.O.times mypatch.O.times | sort -t, -n -k 6 | column -s, -t

import sys
import re

VERBOSE = 0

# #,TEST,SAMPLES,MIN(ms),MAX(ms),MEAN(ms),SD(ms),MEDIAN(ms)
SCORERE = re.compile(r"(\d+),[ \t]*(\w+),[ \t]*([\d.]+),[ \t]*([\d.]+)")
TOTALRE = re.compile(r"()(Totals),[ \t]*([\d.]+),[ \t]*([\d.]+)")
KEYGROUP = 2
VALGROUP = 4
NUMGROUP = 1

IsTime = 1
ShowSpeedup = 1
PrintAllScores = 0

def parseInt(word):
    try:
        return int(word)
    except:
        raise Exception("Expected integer value, not " + word)

def getScores(fname):
    scores = {}
    nums = {}
    runs = 0
    f = open(fname)
    try:
        for line in f:
            if VERBOSE:
                print "Parsing", line,
            m = SCORERE.match(line)
            is_total = False
            if not m:
                is_total = True
                m = TOTALRE.match(line)
            if not m:
                continue

            if VERBOSE:
                print "  match", m.group(KEYGROUP), m.group(VALGROUP)

            if not m.group(KEYGROUP) in scores:
                scores[m.group(KEYGROUP)] = []
            scores[m.group(KEYGROUP)].append(parseInt(m.group(VALGROUP)))
            if is_total:
                nums[m.group(KEYGROUP)] = ""
            else:
                nums[m.group(KEYGROUP)] = m.group(NUMGROUP)
            if len(scores[m.group(KEYGROUP)]) > runs:
                runs = len(scores[m.group(KEYGROUP)])
    finally:
        f.close()
    return scores, runs, nums

def isMaxScore(newscore, maxscore, invert):
    return not maxscore or (newscore > maxscore if not invert else newscore < maxscore)

def compareScores(key, score1, score2, runs, num):
    print num.rjust(3),
    print key.ljust(25),
    bestscore1 = None
    bestscore2 = None
    worstscore1 = None
    worstscore2 = None
    minbest = IsTime
    minworst = not minbest
    r = 0
    for score in score1:
        if isMaxScore(newscore=score, maxscore=bestscore1, invert=minbest):
            bestscore1 = score
        if isMaxScore(newscore=score, maxscore=worstscore1, invert=minworst):
            worstscore1 = score
        if PrintAllScores:
            print ("%d" % score).rjust(16),
    for score in score2:
        if isMaxScore(newscore=score, maxscore=bestscore2, invert=minbest):
            bestscore2 = score
        if isMaxScore(newscore=score, maxscore=worstscore2, invert=minworst):
            worstscore2 = score
        if PrintAllScores:
            print ("%d" % score).rjust(16),
        r += 1
    while r < runs:
        if PrintAllScores:
            print ("0").rjust(9),
        r += 1

    if not PrintAllScores:
        print ("%d" % bestscore1).rjust(16),
        print ("%d" % bestscore2).rjust(16),

    print ("%+d" % (bestscore2 - bestscore1)).rjust(9),

    if bestscore1 != 0 and bestscore2 != 0:
        print ("%+.1f%%" % (((float(bestscore2) / bestscore1) - 1) * 100)).rjust(9),
        if ShowSpeedup:
            Num, Den = float(bestscore2), float(bestscore1)
            if IsTime:
                Num, Den = Den, Num
            print ("%.2fx" % (Num / Den)).rjust(9),
    else:
        print "*".rjust(9),
        if ShowSpeedup:
            print "*".rjust(9),
    # if the interval endpoints have inverse relationship, then they overlap
    if minbest:
        if bestscore1 < worstscore2:
            print "(!)",
    else:
        if bestscore1 > worstscore2:
            print "(!)",
    print

def printBestScores(key, scores):
    print key,
    bestscore = None
    minbest = IsTime
    for score in scores:
        if isMaxScore(newscore=score, maxscore=bestscore, invert=minbest):
            bestscore = score
    print ", %d" % bestscore

def usage():
    print "repeat.sh <n> Benchmark_O[none|unchecked] > file.times"
    print "compare_perf_tests.py <file.times> [<file2.times>]"

if __name__ == '__main__':
    if len(sys.argv) < 2:
        usage()
        sys.exit(1)
    file1 = sys.argv[1]
    if len(sys.argv) < 3:
        scores, runs, nums = getScores(file1)
        keys = list(set(scores.keys()))
        keys.sort()
        for key in keys:
            printBestScores(key, scores[key])
        sys.exit(0)

    file2 = sys.argv[2]
    if len(sys.argv) > 3:
        SCORERE = re.compile(sys.argv[3])

    scores1, runs1, nums = getScores(file1)
    scores2, runs2, nums = getScores(file2)

    runs = runs1
    if runs2 > runs:
        runs = runs2

    if VERBOSE:
        print scores1
        print scores2

    keys = list(set(scores1.keys() + scores2.keys()))
    keys.sort()
    if VERBOSE:
        print "comparing ", file1, "vs", file2, "=",
        if IsTime:
            print file1, "/", file2
        else:
            print file2, "/", file1

    print "#".rjust(3),
    print "TEST".ljust(25),
    if PrintAllScores:
        for i in range(0, runs):
            print ("OLD_RUN%d" % i).rjust(9),
        for i in range(0, runs):
            print ("NEW_RUN%d" % i).rjust(9),
    else:
        print "BEST_OLD_MIN(μs)".rjust(17),
        print "BEST_NEW_MIN(μs)".rjust(17),
    print 'DELTA'.rjust(9), '%DELTA'.rjust(9), 'SPEEDUP'.rjust(9)

    for key in keys:
        if key not in scores1:
            print key, "not in", file1
            continue
        if key not in scores2:
            print key, "not in", file2
            continue
        compareScores(key, scores1[key], scores2[key], runs, nums[key])
