#!/usr/bin/env python3
# optimizer_counters_to_sql.py - Store CSV counters into SQLite  -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import sqlite3 as lite
import sys


# A simple class to connect to a DB and store the statistics from a CSV
# (comma separated values) file with optimizer counters there.
#
# See OptimizerCountersAnalysis.md for more details about working with
# produced SQLite database and analyzing the collected optimizer counters.
class OptStatsDB(object):
    def __init__(self, dbname):
        try:
            # Establish a connection to a DB.
            con = lite.connect(dbname)
            self.con = con
            cur = con.cursor()
            self.cur = cur
            # FIXME: A more efficient representation could be used.
            # There could be separate tables for all possible Stage names,
            # Transform names, Kind names, Counter names, Symbol names.
            # They would get unique integer IDs in their respective tables.
            # The Counters table would then use those unique integer IDs
            # as foreign keys.
            # This way the storage required for Counters may get much
            # smaller as it would only store integers instead of long repeating
            # strings.
            # The drawback of such an approach is that the queries may become
            # much more complex to write, as they would need to span over
            # multiple tables.

            # Create the required tables, indices, etc.
            cur.execute("CREATE TABLE IF NOT EXISTS Counters"
                        "(Id INTEGER PRIMARY KEY AUTOINCREMENT, "
                        "Stage TEXT NOT NULL, "
                        "Transform TEXT NOT NULL, "
                        "Kind TEXT, "
                        "Counter TEXT NOT NULL, "
                        "PassNum INT NOT NULL, "
                        "Delta NUMBER,"
                        "Old INT, "
                        "New INT, "
                        "Duration INT, "
                        "Symbol TEXT NOT NULL DEFAULT '')")
            cur.execute('CREATE INDEX IF NOT EXISTS StageIndex '
                        'ON Counters(Stage)')
            cur.execute('CREATE INDEX IF NOT EXISTS TransformIndex '
                        'ON Counters(Transform)')
            cur.execute('CREATE INDEX IF NOT EXISTS KindIndex '
                        'ON Counters(Kind)')
            cur.execute('CREATE INDEX IF NOT EXISTS CounterIndex '
                        'ON Counters(Counter)')
            cur.execute('CREATE INDEX IF NOT EXISTS SymbolIndex '
                        'ON Counters(Symbol)')
        except lite.Error as e:
            print('Error {}' .format(e.args[0]))
            sys.exit(1)
        finally:
            pass

    def finish(self):
        try:
            self.con.commit()
            self.con.close()
        finally:
            pass

    # Store a single statistics record into a DB.
    def addRecord(self, stage, transform, kind,
                  counter, passnum, delta, old, new, duration, symbol):
        values = [(stage, transform, kind, counter, passnum,
                   delta, old, new, duration, symbol,), ]
        self.cur.executemany(
            'INSERT INTO Counters VALUES '
            '(NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)',
            values)


# Read input file line by line, parse the lines and store the stats into
# the DB.
def addStatsFromInput(inputFile, db):
    for line in inputFile:
        # Read next line
        # Split into segments
        segments = line.split(", ")
        if len(segments) < 6 or not (segments[0] in [
                'module', 'function', 'function_history']):
            continue
        # Trim all values
        segments = list(map(str.strip, segments))
        if segments[0] == 'function_history' or segments[1] == 'lostvars':
            # Process history records
            delta = 0.0
            (kind, counter, stage, transform, passnum,
             old, duration, symbol) = segments
            new = old
        else:
            # Process stats records
            (kind, counter, stage, transform, passnum, delta,
             old, new, duration, symbol) = segments

        db.addRecord(
            stage,
            transform,
            kind,
            counter,
            passnum,
            delta,
            old,
            new,
            duration,
            symbol)


def processStatsFile(filename, dbname):
    print(
        "Copying stats from the file '{}' into database '{}'".format(
            filename,
            dbname))
    db = OptStatsDB(dbname)
    with open(filename, "r") as inputFile:
        addStatsFromInput(inputFile, db)
    db.finish()


def main():
    filename = sys.argv[1]
    dbname = sys.argv[2]
    processStatsFile(filename, dbname)


if __name__ == '__main__':
    main()
