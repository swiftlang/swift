// RUN: %target-swift-remoteast-test %s | %FileCheck %s

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

typealias Fn1 = () -> ()
printType(Fn1.self)
// CHECK: found type: () -> ()

typealias Fn2 = (Int, Float) -> ()
printType(Fn2.self)
// CHECK: found type: (Int, Float) -> ()

typealias Tuple1 = (Int, Float, Int)
printType(Tuple1.self)
// CHECK: found type: (Int, Float, Int)

printType(Int.Type.self)
// CHECK: found type: Int.Type

printType(Tuple1.Type.self)
// CHECK: found type: (Int, Float, Int).Type

typealias Tuple2 = (Int.Type, x: Float, Int)
printType(Tuple2.self)
// CHECK: found type: (Int.Type, x: Float, Int)

typealias Tuple3 = (x: Int, Float, y: Int.Type)
printType(Tuple3.self)
// CHECK: found type: (x: Int, Float, y: Int.Type)
