#!/usr/bin/env bash
#
# This script produces a list of all diagnostics that are defined
# but not used in sources.
#
set -euo pipefail

# Gather all diagnostic identifiers.
ALL_DIAGS=$(
    grep -Eho '(ERROR|WARNING|NOTE|REMARK)\([a-z_]+,' include/swift/AST/Diagnostics*.def \
    | sed -E 's/(ERROR|WARNING|NOTE|REMARK)\(([a-z_]+),/\2/')

# Now look for all potential identifiers in the (C++) source files.
CXX_SOURCES=$(find lib include tools -name \*.cpp -or -name \*.h)
DIAGS_IN_CXX_SOURCES=$(
    grep -Ehoz 'diag::\n?\s*[a-z_]+' $CXX_SOURCES \
    | sed -e 's/diag:://' -e 's/[[:space:]]//g')

# Get potentially unused diags from C++ sources.
POTENTIALLY_UNUSED=$(comm -23 <(sort -u <<< "$ALL_DIAGS") <(sort -u <<< "$DIAGS_IN_CXX_SOURCES"))

# Finally, check if any of the possibly-unused diags appear in Swift sources, and exclude them.
SWIFT_SOURCES=$(find SwiftCompilerSources -name \*.swift)
if [ -n "$SWIFT_SOURCES" ] && [ -n "$POTENTIALLY_UNUSED" ]; then
    DIAGS_IN_SWIFT=$(grep -Fho -f <(echo "$POTENTIALLY_UNUSED") $SWIFT_SOURCES)
    comm -23 <(echo "$POTENTIALLY_UNUSED") <(sort -u <<< "$DIAGS_IN_SWIFT")
else
    echo "$POTENTIALLY_UNUSED"
fi
