// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64e-apple-macos11.0 2>&1 | %FileCheck -check-prefix=CHECK-APPLE-M1 %s
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-ios13.0-simulator 2>&1 | %FileCheck -check-prefix=CHECK-APPLE-M1 %s
// CHECK-APPLE-M1: '-target-cpu' 'apple-m1' '-target-feature'

// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64e-apple-ios13.0 2>&1 | %FileCheck -check-prefix=CHECK-APPLE-A12 %s
// CHECK-APPLE-A12: '-target-cpu' 'apple-a12' '-target-feature'

// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-ios13.0 2>&1 | %FileCheck -check-prefix=CHECK-APPLE-A7 %s
// CHECK-APPLE-A7: '-target-cpu' 'apple-a7' '-target-feature'

// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-ios13.0 -target-cpu apple-a16 2>&1 | %FileCheck -check-prefix=CHECK-APPLE-A16 %s
// CHECK-APPLE-A16: '-target-cpu' 'apple-a16' '-target-feature'
