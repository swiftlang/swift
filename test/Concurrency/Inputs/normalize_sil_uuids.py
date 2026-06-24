"""Normalize SIL output for stable diffs.

Opened-existential archetypes carry a per-compilation UUID that would prevent
diffing SIL. Replace each archetype's UUID with a placeholder before diff.
"""

import re
import sys

sys.stdout.write(
    re.sub(r'@opened\("[^"]+"', '@opened("UUID"', sys.stdin.read())
)
