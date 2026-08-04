// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol ProtocolHasInOut {
  associatedtype Input
  typealias Mutator = (inout Input) -> ()
  var f: Mutator { get }
}

// Emit type metadata for this struct's fields
struct HasInOut<T> {
  var f: (inout T) -> ()
}

struct HasInOutProtocol : ProtocolHasInOut {
  typealias Input = Int
  let f: (inout Input) -> ()
}

func foo<T>(_ t: T.Type) -> Any {
  return { (x: T) -> Int in return 6060 }
}

var i = 0

// Dynamic casting
var f = foo((Int, Int).self)
var g = f as! ((Int, Int)) -> Int
print(g((1010, 2020)))
// CHECK: 6060

// Struct with InOut
let hio = HasInOut(f: { (x: inout Int) in x = 3030 })
i = 0
hio.f(&i)
print(i)
// CHECK: 3030

// Struct that conforms to Protocol with InOut
let hiop = HasInOutProtocol(f: { (x: inout Int) in x = 4040 })
i = 0
hiop.f(&i)
print(i)
// CHECK: 4040

func fooInOut<T>(_ t: T.Type) -> Any {
  return { (x: inout T) -> () in x = unsafeBitCast((8080, 9090), to: T.self) }
}

var fio = fooInOut((Int, Int).self)
var gio = fio as! (inout (Int, Int)) -> ()
var xy = (0, 0)
gio(&xy)
print(xy)
// CHECK: (8080, 9090)
