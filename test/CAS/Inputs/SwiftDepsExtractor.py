#!/usr/bin/env python3
#
# Usage: SwiftDepsExtractor.py file.json ModuleName Key

import json
import sys

input_json = sys.argv[1]
module_name = sys.argv[2]
key = sys.argv[3]

mode = 'swift'
is_bridging_header = False

if module_name.startswith('clang:'):
    mode = 'clang'
    module_name = module_name[6:]
elif module_name.startswith('swiftPrebuiltExternal:'):
    mode = 'swiftPrebuiltExternal'
    module_name = module_name[22:]
elif module_name.startswith('bridgingHeader:'):
    is_bridging_header = True
    module_name = module_name[15:]

with open(input_json, 'r') as file:
    deps = json.load(file)
    module_names = deps['modules'][::2]
    module_details = deps['modules'][1::2]
    for name, detail in zip(module_names, module_details):
        if name.get(mode, '') != module_name:
            continue

        if key in detail.keys():
            json.dump(detail[key], sys.stdout, indent=2)
            break

        if is_bridging_header:
            json.dump(detail['details'][mode]['bridgingHeader']
                      [key], sys.stdout, indent=2)
            break

        json.dump(detail['details'][mode][key], sys.stdout, indent=2)
        break
