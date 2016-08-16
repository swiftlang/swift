#!/usr/bin/env zsh
#===--- find-overlay-dependencies.sh - ...by looking at imported headers ---===#
#
## This source file is part of the Swift.org open source project
##
## Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
## Licensed under Apache License v2.0 with Runtime Library Exception
##
## See http://swift.org/LICENSE.txt for license information
## See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===------------------------------------------------------------------------===#

# This script uses zsh for its associative array support, because it is only
# intended to be run on macOS and macOS bash is very old.

if [[ $# -ne 1 ]]; then
  echo 'usage:' $0 '<module-name>' >&2
  exit 1
fi

OVERLAYS_PATH=$(dirname "$0")/../stdlib/public/SDK/

typeset -A CUSTOM_NAMED_MODULES
CUSTOM_NAMED_MODULES[ObjectiveC]=objc
CUSTOM_NAMED_MODULES[Dispatch]=dispatch
CUSTOM_NAMED_MODULES[XPC]=xpc

ALL_OVERLAYS=()
for overlay in $(ls "$OVERLAYS_PATH"); do
  ALL_OVERLAYS+=${CUSTOM_NAMED_MODULES[$overlay]-$overlay}
done

typeset -A SDKS
SDKS[macosx]=x86_64
SDKS[iphoneos]=arm64
SDKS[appletvos]=arm64
SDKS[watchos]=armv7k

for sdk in ${(k)SDKS}; do
  arch=$SDKS[$sdk]
  printf "%s:\n\t" "$sdk"
  deps=$(echo "@import $1;" | xcrun -sdk $sdk clang -arch $arch -x objective-c - -M -fmodules 2>/dev/null)
  for overlay in $ALL_OVERLAYS; do
    (echo "$deps" |
        egrep -v 'module.(module)?map' |
        egrep -v "\b${CUSTOM_NAMED_MODULES[$1]-$1}\b" |
        egrep -q "\b$overlay\b") &&
      printf "%s " $overlay
  done
  echo # newline
done
