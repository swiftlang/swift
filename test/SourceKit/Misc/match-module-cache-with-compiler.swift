// This test ensures a certain set of arguments allows both compiler and sourcekitd invocations to share the same module cache.
// NOTE: Do not change this test without a review from @akyrtzi

// https://github.com/swiftlang/swift/issues/86930
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)

// DEFINE: %{args} = \
// DEFINE:   -module-name themod \
// DEFINE:   -module-cache-path %t/mcp \
// DEFINE:   -sdk %sdk \
// DEFINE:   -swift-version 5 \
// DEFINE:   -I %S/Inputs/mymod \
// DEFINE:   -Xfrontend -experimental-allow-module-with-compiler-errors \
// DEFINE:   -Xcc -D__INDEX_BUILD__=1 -D__INDEX_BUILD__ \
// DEFINE:   -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors \
// DEFINE:   -Xcc -Xclang -Xcc -fmodule-format=raw \
// DEFINE:   -Xcc -Xclang -Xcc -detailed-preprocessing-record \
// DEFINE:   %s

// RUN: %swiftc_driver -emit-module -emit-module-path themod.swiftmodule -Xfrontend -experimental-skip-all-function-bodies  %{args}
// RUN: %swiftc_driver -index-file -index-file-path %s -index-store-path %t/idx -index-ignore-system-modules %{args}
// RUN: %sourcekitd-test -req=sema %s -- %{args}
// RUN: %find_files %t%{fs-sep}mcp 'mymod-*.pcm' | count 1

import mymod
