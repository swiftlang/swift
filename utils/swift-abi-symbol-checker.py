#!/usr/bin/env python3

import argparse
import sys


def getAdditionsAndRemovals(changesFile):
    changesF = open(changesFile)
    changes = changesF.read()
    changesF.close()

    # Get rid of lines that start with either '//' or a newline
    changes = [c for c in changes.splitlines() if not c.startswith('//') and c != '']

    # Filter the changes for lines that start with Added
    additions = [a for a in changes if a.startswith('Added')]
    # Filter the changes for lines that start with Removed
    removals = [r for r in changes if r.startswith('Removed')]

    # Map the additions by removing the 'Added: ' prefix to get just the symbol
    additions = list(map(lambda a: a.removeprefix('Added: '), additions))
    # Map the removals by removing the 'Removed: ' prefix to get just the symbol
    removals = list(map(lambda r: r.removeprefix('Removed: '), removals))

    return (additions, removals)


def checkSymbols(args):
    # If we were passed a base file, read those changes first. This most likely
    # occurs in assert configurations getting the non-assert base.
    baseAdditions = []
    baseRemovals = []

    if args.base:
        (baseAdditions, baseRemovals) = getAdditionsAndRemovals(args.base)

    (additions, removals) = getAdditionsAndRemovals(args.changes)

    # Append the base additions and removals to ours.
    additions.extend(baseAdditions)
    removals.extend(baseRemovals)

    # We need to write back to the temporary symbol file for diffing
    symbolsF = open(args.symbols, 'r+')

    symbols = symbolsF.read().splitlines()

    # Check for added symbols that are not actually in the just built dylib.
    notInDylib = [a for a in additions if a not in symbols]

    # If there were symbols marked as 'Added' in the changes file, but they didn't
    # actually appear in the dylib then print those symbols out and fail.
    if notInDylib:
        for symbol in notInDylib:
            symbols.append('<MISSING ADDITION> {}'.format(symbol))

    # Filter the built symbols for the additions because we're removing them to
    # get back to the baseline
    symbols = [s for s in symbols if s not in additions]

    # Append the removals into the symbol list to get back to the baseline
    symbols.extend(removals)

    # Sort the end result to write back
    symbols.sort()

    # Go back to beginning of the file and purge everything
    symbolsF.seek(0)
    symbolsF.truncate()

    # Append a newline to each symbol (because writelines doesn't do that for us)
    symbols = list(map(lambda s: s + '\n', symbols))

    # Write all of our symbols back into the symbols file
    symbolsF.writelines(symbols)

    # Done
    symbolsF.close()


def main(arguments):
    parser = argparse.ArgumentParser(
        description='Change absolute install names to use @rpath')

    parser.add_argument('changes', help='the changes file')
    parser.add_argument('symbols', help='the symbols file')
    parser.add_argument('--base', help='the base changes file')

    args = parser.parse_args(arguments)

    checkSymbols(args)


sys.exit(main(sys.argv[1:]))
