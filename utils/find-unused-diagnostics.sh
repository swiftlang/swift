#!/usr/bin/env bash
#
# This script produces a list of all diagnostics that are defined
# but not used in sources.
#
set -euo pipefail

# Gather all diagnostic identifiers.
ALL_DIAGS=$(
    grep -E --only-matching --no-filename '(ERROR|WARNING|NOTE|REMARK)\([a-zA-Z0-9_]+,' include/swift/AST/Diagnostics*.def \
    | sed -E 's/(ERROR|WARNING|NOTE|REMARK)\(([a-zA-Z0-9_]+),/\2/')

# Now look for all potential identifiers in the (C++) source files.
CXX_SOURCES=$(find lib include tools -name \*.cpp -or -name \*.h)
DIAGS_IN_CXX_SOURCES=$(
    grep -E --only-matching --no-filename --null-data 'diag::\n?\s*[a-zA-Z0-9_]+' $CXX_SOURCES \
    | sed -e 's/diag:://' -e 's/[[:space:]]//g')

# Get potentially unused diags from C++ sources.
POTENTIALLY_UNUSED=$(comm -23 <(sort -u <<< "$ALL_DIAGS") <(sort -u <<< "$DIAGS_IN_CXX_SOURCES"))

# Finally, check if any of the possibly-unused diags appear in Swift sources, and exclude them.
SWIFT_SOURCES=$(find SwiftCompilerSources -name \*.swift)
if [ -n "$SWIFT_SOURCES" ] && [ -n "$POTENTIALLY_UNUSED" ]; then
    DIAGS_IN_SWIFT=$(grep --fixed-strings --only-matching --no-filename --file <(echo "$POTENTIALLY_UNUSED") $SWIFT_SOURCES)
    comm -23 <(echo "$POTENTIALLY_UNUSED") <(sort -u <<< "$DIAGS_IN_SWIFT")
else
    echo "$POTENTIALLY_UNUSED"
fi
