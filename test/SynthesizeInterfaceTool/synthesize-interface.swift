// RUN: %target-swift-synthesize-interface -module-name m1 -I %S/Inputs -o - | %FileCheck %s

// CHECK:     public struct MyStruct {
// CHECK-DAG:     public init()
// CHECK-DAG:     public init(value: Int32)
// CHECK-DAG:     public var value: Int32
// CHECK-DAG: }
// CHECK-DAG: extension MyStruct {
// CHECK-DAG:     public func printValue()
// CHECK-DAG: }
