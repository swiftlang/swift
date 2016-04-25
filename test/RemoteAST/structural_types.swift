// RUN: %target-swift-remoteast-test %s | FileCheck %s

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

typealias Fn1 = () -> ()
printType(Fn1.self)
// CHECK: () -> ()

typealias Fn2 = (Int, Float) -> ()
printType(Fn2.self)
// CHECK: (Int, Float) -> ()

typealias Tuple1 = (Int, Float, Int)
printType(Tuple1.self)
// CHECK: (Int, Float, Int)

printType(Int.Type.self)
// CHECK: Int.Type

printType(Tuple1.Type.self)
// CHECK: (Int, Float, Int).Type
