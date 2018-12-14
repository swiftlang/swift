// RUN: %target-swift-remoteast-test %s | %FileCheck %s

// REQUIRES: swift-remoteast-test

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

typealias Fn1 = () -> ()
printType(Fn1.self)
// CHECK: found type: () -> ()

typealias Fn2 = (Int, Float) -> ()
printType(Fn2.self)
// CHECK: found type: (Int, Float) -> ()

typealias Fn3 = (inout Int, Float) -> ()
printType(Fn3.self)
// CHECK: found type: (inout Int, Float) -> ()

typealias Fn4 = (inout Int, inout Float) -> ()
printType(Fn4.self)
// CHECK: found type: (inout Int, inout Float) -> ()

typealias Fn5 = (Int, inout Float) -> ()
printType(Fn5.self)
// CHECK: found type: (Int, inout Float) -> ()

typealias Fn6 = (Int, inout String, Float) -> ()
printType(Fn6.self)
// CHECK: found type: (Int, inout String, Float) -> ()

typealias Fn7 = (inout Int, String, inout Float, Double) -> ()
printType(Fn7.self)
// CHECK: found type: (inout Int, String, inout Float, Double) -> ()

typealias Fn8 = (String, Int, Double, Float) -> ()
printType(Fn8.self)
// CHECK: found type: (String, Int, Double, Float) -> ()

typealias Fn9 = ((Int, Float)) -> ()
printType(Fn9.self)
// CHECK: found type: ((Int, Float)) -> ()

typealias Fn10 = (Int...) -> ()
printType(Fn10.self)
// CHECK: found type: (Int...) -> ()

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

func foo<T>(_: T) {
  var f = T.self
  printType(f)
}

foo() { (x: Int) -> Int in return x }
// CHECK: found type: (Int) -> Int
