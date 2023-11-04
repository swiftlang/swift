#!/usr/bin/env python3
#
# Usage: ExtractOutputKey.py file.json OutputPath

import json
import sys

input_json = sys.argv[1]
output_path = sys.argv[2]


with open(input_json, 'r') as file:
    entries = json.load(file)
    for entry in entries:
        for output in entry["Outputs"]:
            if output['Path'] != output_path:
                continue
            print(entry['CacheKey'])
