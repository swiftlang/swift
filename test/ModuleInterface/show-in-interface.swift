// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: @_show_in_interface public protocol _UnderscoredProtocol {
// CHECK-NEXT: }
@_show_in_interface
public protocol _UnderscoredProtocol {}
