#!/usr/bin/env python3

# Prints the full declaration of each symbol in a symbol graph, one per line,
# after the symbol's path, e.g. `S.x: var x: Int { get }`. This lets a test
# check many declarations without matching each fragment's JSON separately.

import json
import sys

with open(sys.argv[1]) as graph_file:
    graph = json.load(graph_file)

for symbol in graph['symbols']:
    path = '.'.join(symbol['pathComponents'])
    declaration = ''.join(
        fragment['spelling'] for fragment in symbol['declarationFragments'])
    print(path + ': ' + declaration)
