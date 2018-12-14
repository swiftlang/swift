// Test the fallback for 32-bit ARM platforms.

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/empty.framework/Modules/empty.swiftmodule
// RUN: %target-swift-frontend -emit-module -o %t/empty.framework/Modules/empty.swiftmodule/arm.swiftmodule %S/../Inputs/empty.swift -module-name empty
// RUN: %target-swift-frontend -typecheck %s -F %t

// RUN: mv %t/empty.framework/Modules/empty.swiftmodule/arm.swiftmodule %t/empty.framework/Modules/empty.swiftmodule/%target-swiftmodule-name
// RUN: touch %t/empty.framework/Modules/empty.swiftmodule/arm.swiftmodule
// RUN: %target-swift-frontend -typecheck %s -F %t

// RUN: rm %t/empty.framework/Modules/empty.swiftmodule/%target-swiftmodule-name
// RUN: not %target-swift-frontend -typecheck %s -F %t 2>&1 | %FileCheck %s

// REQUIRES: CPU=armv7 || CPU=armv7k || CPU=armv7s

import empty
// CHECK: :[[@LINE-1]]:8: error: malformed module file: {{.*}}arm.swiftmodule
