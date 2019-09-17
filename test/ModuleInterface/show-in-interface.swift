// RUN: %target-swift-frontend -typecheck %s -parse-stdlib -emit-module-interface-path - | %FileCheck %s

// CHECK: @_show_in_interface public protocol _UnderscoredProtocol {
// CHECK-NEXT: }
@_show_in_interface
public protocol _UnderscoredProtocol {}
