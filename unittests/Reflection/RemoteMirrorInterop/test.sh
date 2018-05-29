#!/bin/bash

# Exercise the SwiftRemoteMirrorLegacyInterop API. This requires a build
# of an old (circa 4.0.3) version of Swift as well as the current one.
# It then builds Swift code using the old and new compilers, and
# exercises the Interop API using the old and new remote mirrors
# libraries.

set -euo pipefail

if [[ $# -ne 4 ]]; then
  echo -n "Usage: $0 <swiftc4-path> <swiftc5-path> "
  echo "<libswiftRemoteMirror4.dylib> <libswiftRemoteMirror5.dylib>"
  exit 1
fi

swiftc4="$1"
swiftc5="$2"
mirrorlib4="$3"
mirrorlib5="$4"

SDK=`xcrun --show-sdk-path`

cd `dirname $0`

"$swiftc4" -sdk "$SDK" -emit-library test4.swift -o /tmp/libtest4.dylib
"$swiftc5" -sdk "$SDK" -emit-library test5.swift -o /tmp/libtest5.dylib

clang -framework Foundation -I ../../../include/swift/SwiftRemoteMirror \
      -o /tmp/test -g test.m 


echo "Testing 4 with both mirror libs"
/tmp/test /tmp/libtest4.dylib "$mirrorlib4" "$mirrorlib5"
echo ""

echo "Testing 4 with only mirror lib 5"
/tmp/test /tmp/libtest4.dylib "-" "$mirrorlib5"
echo ""

echo "Testing 4 with only mirror lib 4"
/tmp/test /tmp/libtest4.dylib "$mirrorlib4" "-"
echo ""

echo "Testing 4 with no mirror libs"
/tmp/test /tmp/libtest4.dylib "-" "-" || true
echo ""

echo "Testing 5 with both mirror libs"
/tmp/test /tmp/libtest5.dylib "$mirrorlib4" "$mirrorlib5"
echo ""

echo "Testing 5 with only mirror lib 5"
/tmp/test /tmp/libtest5.dylib "-" "$mirrorlib5"
echo ""

# Not supported (yet?)
#echo "Testing 5 with only mirror lib 4"
#/tmp/test /tmp/libtest5.dylib "$mirrorlib4" "-"
#echo ""

echo "Testing 5 with no mirror libs"
/tmp/test /tmp/libtest5.dylib "-" "-" || true
