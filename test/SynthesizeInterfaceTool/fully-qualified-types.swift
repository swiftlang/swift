// RUN: %target-swift-synthesize-interface -module-name m1 -print-fully-qualified-types -I %S/Inputs -o - | %FileCheck %s

// CHECK:     public struct MyStruct {
// CHECK-DAG:     public init()
// CHECK-DAG:     public init(value: Swift.Int32)
// CHECK-DAG:     public var value: Swift.Int32
// CHECK-DAG: }
// CHECK-DAG: extension m1.MyStruct {
// CHECK-DAG:     public func printValue()
// CHECK-DAG: }
