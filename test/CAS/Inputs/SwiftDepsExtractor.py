#!/usr/bin/env python3

import json
import sys

## SwiftDepsExtractor.py file.json ModuleName Key
input_json = sys.argv[1]
module_name = sys.argv[2]
key = sys.argv[3]

mode = "swift"

if module_name.startswith("clang:"):
    mode = "clang"
    module_name = module_name[6:]
elif module_name.startswith("swiftPrebuiltExternal:"):
    mode = "swiftPrebuiltExternal"
    module_name = module_name[22:]

with open(input_json, 'r') as file:
    deps = json.load(file)
    extract_next = False
    for module in deps['modules']:
        if extract_next:
            if key in module.keys():
                print(module[key])
                break
            print(module['details'][mode][key])
            break

        if module.get(mode, '') == module_name:
            extract_next = True
