// Test the fallback for arm64e platforms.

// RUN: %empty-directory(%t)
// RUN: mkdir %t/empty.swiftmodule
// RUN: %target-swift-frontend -emit-module -o %t/empty.swiftmodule/arm64.swiftmodule %S/../Inputs/empty.swift -module-name empty
// RUN: %target-swift-frontend -typecheck %s -I %t

// RUN: mv %t/empty.swiftmodule/arm64.swiftmodule %t/empty.swiftmodule/%target-swiftmodule-name
// RUN: touch %t/empty.swiftmodule/arm64.swiftmodule
// RUN: %target-swift-frontend -typecheck %s -I %t

// RUN: rm %t/empty.swiftmodule/%target-swiftmodule-name
// RUN: not %target-swift-frontend -typecheck %s -I %t 2>&1 | %FileCheck %s

// REQUIRES: CPU=arm64e

import empty
// CHECK: :[[@LINE-1]]:8: error: malformed compiled module: {{.*}}arm64.swiftmodule

