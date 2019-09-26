# RUN: %swift_build_sdk_interfaces -machine-parseable-monotonic-version | \
# RUN:   %{python} %s

import sys

lines = list(sys.stdin)
assert len(lines) == 1
assert int(lines[0]) > 0
