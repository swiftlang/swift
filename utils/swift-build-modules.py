#!/usr/bin/env python3

# This tool helps building swift explicit module from the JSON output of the scan-dependencies command. It will build all the module dependencies from JSON and construct a response file for the common arguments for main module build.
# Usage:
#   /path/to/bin/dir/swift-build-modules.py /path/to/swift-frontend /path/to/depscan.json /path/to/output-resp
#

import argparse
import json
import os
import subprocess
import sys


def writeOutputResponseFile(filename, cmd):
    with open(filename, 'w') as output:
        for c in cmd:
            output.write('"{}"\n'.format(c))


def build_module(swift_frontend, mode, detail):
    cmd = [swift_frontend] + detail['details'][mode]['commandLine']
    subprocess.check_call(cmd)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('swift_frontend', help="path to swift-frontend")
    parser.add_argument('input', help="path to json output from scan-dependencies")
    parser.add_argument('-c', '--cas', metavar='<CAS directory>')
    parser.add_argument('--llvm-cas-tool', metavar='<path>', default="llvm-cas")
    parser.add_argument('-o', '--output', metavar="<output>",
                        help="output response file for building main module")
    parser.add_argument('-b', '--bridging-header-resp', metavar="<response file>",
                        help="output response file for building bridging header")
    args = parser.parse_args()

    with open(args.input, 'r') as file:
        # Read input json file.
        deps = json.load(file)
        modules = []
        # Traverse the module name and detail pair in reverse order assuming that is the order of dependencies.
        # Skip the first module since that is the main module.
        module_names = reversed(deps['modules'][2::2])
        module_details = reversed(deps['modules'][3::2])
        for name, detail in zip(module_names, module_details):
            module = {}
            module["isFramework"] = False
            if 'clang' in name:
                build_module(args.swift_frontend, 'clang', detail)
                module["moduleName"] = name['clang']
                if "moduleCacheKey" in detail["details"]['clang']:
                    module["clangModuleCacheKey"] = detail["details"]['clang']["moduleCacheKey"]
                    module["clangModulePath"] = os.path.basename(detail["modulePath"])
                else:
                    module["clangModulePath"] = detail["modulePath"]
            if 'swift' in name:
                build_module(args.swift_frontend, 'swift', detail)
                module["moduleName"] = name['swift']
                if "moduleCacheKey" in detail["details"]['swift']:
                    module["moduleCacheKey"] = detail["details"]['swift']["moduleCacheKey"]
                    module["modulePath"] = os.path.basename(detail["modulePath"])
                else:
                    module["modulePath"] = detail["modulePath"]
            if 'swiftPrebuiltExternal' in name:
                module["moduleName"] = name['swiftPrebuiltExternal']
                if "moduleCacheKey" in detail["details"]['swiftPrebuiltExternal']:
                    module["moduleCacheKey"] = detail["details"]['swiftPrebuiltExternal']["moduleCacheKey"]
                    module["modulePath"] = os.path.basename(detail["modulePath"])
                else:
                    module["modulePath"] = detail["modulePath"]
            modules.append(module)

        # Write output response file if requested.
        if args.output:
            cmd = deps['modules'][1]['details']['swift']['commandLine']
            # Add some helpful flags for explicit module build.
            cmd.extend(['-disable-implicit-swift-modules'])
            # Write explicit module map.
            module_map_out = args.output + ".map"
            with open(module_map_out, 'w') as mapfile:
                json.dump(modules, mapfile, indent=2)
            # If using caching, create the map in CAS.
            if args.cas:
                casid = subprocess.check_output(
                    [args.llvm_cas_tool, '--cas', args.cas, '--make-blob', '--data', module_map_out], text=True).strip()
                cmd.extend(['-explicit-swift-module-map-file', casid])
            else:
                cmd.extend(['-explicit-swift-module-map-file', module_map_out])
            writeOutputResponseFile(args.output, cmd)

        # Write bridging header response file if request.
        if args.bridging_header_resp:
            info = deps['modules'][1]['details']['swift']
            # the first argument is `-frontend`
            cmd = info['bridgingHeader']['commandLine'][1:]
            # print input file name if using chained bridging header.
            if "chainedBridgingHeaderPath" in info:
                cmd.append(info['chainedBridgingHeaderPath'])
            writeOutputResponseFile(args.bridging_header_resp, cmd)


if __name__ == '__main__':
    main()
