// RUN: %target-swift-synthesize-interface -cxx-interoperability-mode=default -module-name mcxx -I %S/Inputs -o - | %FileCheck %s

// CHECK:     public struct MyClass {
// CHECK-DAG:     public init(_ v: Int32)
// CHECK-DAG:     public init()
// CHECK-DAG:     public func printValue()
// CHECK-DAG: }
