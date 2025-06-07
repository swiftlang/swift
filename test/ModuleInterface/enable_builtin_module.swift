// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -enable-builtin-module
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: -enable-builtin-module

// CHECK: import Builtin
// CHECK: import Swift
// CHECK: import _Concurrency
// CHECK: import _StringProcessing
// CHECK: import _SwiftConcurrencyShims

// CHECK: public func something(with x: Builtin.RawPointer)

import Builtin

public func something(with x: Builtin.RawPointer) {}
