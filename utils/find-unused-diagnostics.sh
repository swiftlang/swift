#!/usr/bin/env bash
#
# This script produces a list of all diagnostics that are defined
# but not used in sources.
#

# Gather all diagnostic identifiers.
ALL_DIAGS=$(grep -E --only-matching --no-filename 'ERROR\([a-z_]+,' include/swift/AST/Diagnostics*.def | sed -e 's/ERROR(//' -e 's/,//')

# Now look for all potential identifiers in the source files.
ALL_SOURCES=$(find lib include tools -name \*.cpp -or -name \*.h)
DIAGS_IN_SOURCES=$(grep -E --only-matching --no-filename 'diag::[a-z_]+' $ALL_SOURCES | sed -e 's/diag:://')

# Print all diags that occur in the .td files but not in the source.
comm -23 <(sort -u <<< "$ALL_DIAGS") <(sort -u <<< "$DIAGS_IN_SOURCES")
