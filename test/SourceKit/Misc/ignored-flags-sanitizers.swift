var s = 10
s.

// CHECK: littleEndian

// REQUIRES: asan_runtime
// REQUIRES: fuzzer_runtime

// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -sanitize=address,fuzzer -sanitize-coverage=func %s | %FileCheck %s
