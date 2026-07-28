// RUN: %target-swift-synthesize-interface -module-name m1 -I %S/Inputs -o - | %FileCheck %s
// RUN: %swift-synthesize-interface -module-name m1 -I %S/Inputs -o - | %FileCheck %s

// CHECK:     public struct MyStruct {
// CHECK-DAG:     public init()
// CHECK-DAG:     public init(value: CInt)
// CHECK-DAG:     public var value: CInt
// CHECK-DAG: }
// CHECK-DAG: extension MyStruct {
// CHECK-DAG:     public func printValue()
// CHECK-DAG: }
