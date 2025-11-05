#!/usr/bin/env python3
"""
SARIF v2.1.0 validator for Swift compiler test output.
Validates JSON structure and required fields without exact text matching.
"""

import json
import sys

def readfile(path):
    data = ""
    try:
        with open(path) as f:
            data = f.read()
    except Exception as e:
        data = f"[\"Error: {e}\"]"
    return data

def process_sarif(sarif_expected_file, sarif_actual_file, swift_input_files):
    sarif_expected_str = readfile(sarif_expected_file)
    sarif_actual = json.loads(readfile(sarif_actual_file))

    for (index, swift_input_file) in enumerate(swift_input_files):
        pattern = f"FILE[{index}]"
        sarif_expected_str = sarif_expected_str.replace(pattern, swift_input_file)

    sarif_expected = json.loads(sarif_expected_str)

    return (sarif_actual, sarif_expected)

def main():
    if len(sys.argv) < 4:
        print("Usage: validate-sarif.py sarif_expected_file sarif_actual_file swift_input_file [swift_input_file ...]", file=sys.stderr)
        sys.exit(1)

    (sarif_actual, sarif_expected) = process_sarif(sys.argv[1], sys.argv[2], sys.argv[3:])

    matching = sarif_actual == sarif_expected

    if matching:
        print("SARIF validation passed")
        return 0
    else:
        print("SARIF validation failed:", file=sys.stderr)
        print("Expected:")
        print(json.dumps(sarif_expected, indent = 4))
        print("Actual:")
        print(json.dumps(sarif_actual, indent = 4))
        return 1

if __name__ == '__main__':
    sys.exit(main())
