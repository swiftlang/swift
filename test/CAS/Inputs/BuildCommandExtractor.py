#!/usr/bin/env python3
#
# Usage: BuildCommandExtractor.py file.json ModuleName

import json
import sys

input_json = sys.argv[1]
module_name = sys.argv[2]

mode = "swift"

if module_name.startswith("clang:"):
    mode = "clang"
    module_name = module_name[6:]
elif module_name.startswith("swiftPrebuiltExternal:"):
    mode = "swiftPrebuiltExternal"
    module_name = module_name[22:]


def printCmd(cmd):
    for c in cmd:
        print('"{}"'.format(c))


with open(input_json, "r") as file:
    deps = json.load(file)
    if module_name == "bridgingHeader":
        info = deps["modules"][1]["details"]["swift"]
        # the first argument is `-frontend`
        cmd = info["bridgingHeader"]["commandLine"][1:]
        printCmd(cmd)
        # print input file name.
        print(info["chainedBridgingHeaderPath"])
    else:
        module_names = deps["modules"][::2]
        module_details = deps["modules"][1::2]
        for name, detail in zip(module_names, module_details):
            if name.get(mode, "") != module_name:
                continue

            printCmd(detail["details"][mode]["commandLine"])
