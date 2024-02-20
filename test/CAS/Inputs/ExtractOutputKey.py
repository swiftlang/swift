#!/usr/bin/env python3
#
# Usage: ExtractOutputKey.py file.json InputPath

import json
import sys

input_json = sys.argv[1]
input_path = sys.argv[2]


with open(input_json, 'r') as file:
    entries = json.load(file)
    for entry in entries:
        if entry['Input'] != input_path:
            continue
        print(entry['CacheKey'])
