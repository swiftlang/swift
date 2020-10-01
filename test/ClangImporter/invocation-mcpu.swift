// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64e-apple-ios13.0 2>&1 | %FileCheck -check-prefix=CHECK-APPLE-A12 %s
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64e-apple-macos11.0 2>&1 | %FileCheck -check-prefix=CHECK-APPLE-A12 %s
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64e-apple-ios13.0-simulator 2>&1 | %FileCheck -check-prefix=CHECK-APPLE-A12 %s
// CHECK-APPLE-A12: '-mcpu=apple-a12'

// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-ios13.0 2>&1 | %FileCheck -check-prefix=CHECK-APPLE-A7 %s
// CHECK-APPLE-A7: '-mcpu=apple-a7'
