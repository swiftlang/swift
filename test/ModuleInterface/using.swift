// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s

// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)

// RUN: %FileCheck %s --input-file %t.swiftinterface

using @MainActor

// CHECK-NOT: using @MainActor
