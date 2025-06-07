#!/usr/bin/env python3
#
# Usage: ExtractOutputKey.py file.json InputPath

import json
import sys

input_json = sys.argv[1]


with open(input_json, 'r') as file:
    entries = json.load(file)
    if len(sys.argv) == 2:
        print(entries[0]['CacheKey'])
        exit(0)

    for entry in entries:
        if entry['Input'] != sys.argv[2]:
            continue
        print(entry['CacheKey'])
