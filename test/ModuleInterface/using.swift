// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -enable-experimental-feature DefaultIsolationPerFile

// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)

// RUN: %FileCheck %s --input-file %t.swiftinterface

// REQUIRES: swift_feature_DefaultIsolationPerFile

using @MainActor

// CHECK-NOT: using @MainActor
