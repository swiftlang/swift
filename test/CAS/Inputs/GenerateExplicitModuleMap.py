#!/usr/bin/env python3
#
# Usage: GenerateExplicitModuleMap.py file.json

import json
import sys

input_json = sys.argv[1]

modules = []

with open(input_json, "r") as file:
    deps = json.load(file)
    main_module_name = deps["mainModuleName"]
    module_names = deps["modules"][::2]
    module_details = deps["modules"][1::2]
    # add all modules other than the main module into the module map.
    for name, detail in zip(module_names, module_details):
        kind, name = list(name.items())[0]
        if name == main_module_name:
            continue

        module = {}
        module["moduleName"] = name
        module["isFramework"] = False
        if kind == "clang":
            module["clangModulePath"] = detail["modulePath"]
            if "moduleCacheKey" in detail["details"][kind]:
                module["clangModuleCacheKey"] = detail["details"][kind][
                    "moduleCacheKey"
                ]
        else:
            module["modulePath"] = detail["modulePath"]
            if "moduleCacheKey" in detail["details"][kind]:
                module["moduleCacheKey"] = detail["details"][kind]["moduleCacheKey"]

        modules.append(module)


json.dump(modules, sys.stdout, indent=2)
