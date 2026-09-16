#!/usr/bin/env python3
"""Emit a mangled name whose identifier expands past the demangler's limit.

`0` enters word-substitution mode, the first identifier seeds Words[0] with a
WORD_LENGTH-character word, and each following lowercase letter re-appends that
whole word. WORD_LENGTH * SUBSTITUTIONS is exactly 4 MiB.
"""

import sys

WORD_LENGTH = 2048
SUBSTITUTIONS = int(sys.argv[1]) if len(sys.argv) > 1 else 2048

print("$s0" + str(WORD_LENGTH) + ("x" * WORD_LENGTH)
      + ("a" * SUBSTITUTIONS) + "0" + "3foo" + "V")
