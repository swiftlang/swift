// Emit the explicit module.
// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-pcm -module-name m1 -o %t/m1.pcm %S/Inputs/module.modulemap

// Invoke the frontend with the PCM as an input.
// RUN: %target-swift-synthesize-interface %windows_vfs_overlay_opt -module-name m1 -Xcc -fmodule-file=%t/m1.pcm -o - | %FileCheck %s

// CHECK:     public struct MyStruct {
// CHECK-DAG:     public init()
// CHECK-DAG:     public init(value: Int32)
// CHECK-DAG:     public var value: Int32
// CHECK-DAG: }
// CHECK-DAG: extension MyStruct {
// CHECK-DAG:     public func printValue()
// CHECK-DAG: }
