// This test ensures a certain set of arguments allows both compiler and sourcekitd invocations to share the same module cache.
// NOTE: Do not change this test without a review from @akyrtzi

// REQUIRES: shell

// https://github.com/apple/swift/issues/58786
// UNSUPPORTED: OS=linux-gnu

// RUN: %empty-directory(%t)

// RUN: COMPILER_ARGS=( \
// RUN:   -module-name themod \
// RUN:   -module-cache-path %t/mcp \
// RUN:   -sdk %sdk \
// RUN:   -swift-version 5 \
// RUN:   -I %S/Inputs/mymod \
// RUN:   -Xfrontend -experimental-allow-module-with-compiler-errors \
// RUN:   -Xcc -D__INDEX_BUILD__=1 -D__INDEX_BUILD__ \
// RUN:   -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors \
// RUN:   -Xcc -Xclang -Xcc -fmodule-format=raw \
// RUN:   -Xcc -Xclang -Xcc -detailed-preprocessing-record \
// RUN:   %s \
// RUN: )

// RUN: %swiftc_driver -emit-module -emit-module-path themod.swiftmodule -Xfrontend -experimental-skip-all-function-bodies ${COMPILER_ARGS[@]}
// RUN: %swiftc_driver -index-file -index-file-path %s -index-store-path %t/idx -index-ignore-system-modules ${COMPILER_ARGS[@]}
// RUN: %sourcekitd-test -req=sema %s -- ${COMPILER_ARGS[@]}
// RUN: find %t/mcp -name "mymod-*.pcm" | count 1

import mymod
