#!/usr/bin/env python3
"""Utility to locate unused diagnostic IDs in the Swift compiler.

The Swift compiler defines diagnostics with unique integer IDs in
`diagnostic` files (e.g. `Diagnostics/*.def`).  Over time some IDs become
orphaned – they are defined but never emitted.  This script scans the
source tree for `diagnostic` definitions and for all call‑sites where a
`diagnostic` is emitted (via `diagnose(...)` or similar).  It then prints
any IDs that appear in the definition files but have no corresponding
emission.

Usage::
    python3 utils/find_unused_diagnostics.py <swift-root>

The script intentionally avoids heavy parsing; it uses simple regexes that
are sufficient for the current codebase.  It can be extended to handle
new patterns as the compiler evolves.
"""

import re
import sys
from pathlib import Path
from collections import defaultdict

# Regexes for definition and emission sites – tuned for the current Swift codebase.
DEF_REGEX = re.compile(r"^\s*def\s+(?P<name>\w+)\s+\(\s*ID\s*=\s*(?P<id>\d+)", re.MULTILINE)
EMIT_REGEX = re.compile(r"diagnose\s*\(\s*\w+\s*\)\s*\{[^}]*?\b(?P<name>\w+)\b", re.DOTALL)

def collect_definitions(root: Path):
    defs = {}
    for file in root.rglob('*.def'):
        content = file.read_text(errors='ignore')
        for match in DEF_REGEX.finditer(content):
            name = match.group('name')
            diag_id = int(match.group('id'))
            defs[name] = diag_id
    return defs

def collect_emissions(root: Path):
    used = set()
    for file in root.rglob('*.swift'):
        try:
            content = file.read_text(errors='ignore')
        except Exception:
            continue
        for match in EMIT_REGEX.finditer(content):
            used.add(match.group('name'))
    return used

def main():
    if len(sys.argv) != 2:
        print('Usage: python3 utils/find_unused_diagnostics.py <swift-root>')
        sys.exit(1)
    root = Path(sys.argv[1])
    definitions = collect_definitions(root)
    emissions = collect_emissions(root)
    unused = {name: diag_id for name, diag_id in definitions.items() if name not in emissions}
    if not unused:
        print('No unused diagnostic IDs found.')
    else:
        print('Unused diagnostic IDs:')
        for name, diag_id in sorted(unused.items(), key=lambda x: x[1]):
            print(f'  {name}: {diag_id}')

if __name__ == '__main__':
    main()
