// RUN: %target-run-simple-swift | FileCheck %s

protocol ProtocolHasInOut {
  typealias Input
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

func foo<T>(t: T.Type) -> Any {
  return { (x: T) -> Int in return 6060 }
}

var i = 0

// Dynamic casting
var f = foo((Int, Int).self)
var g = f as! (Int, Int) -> Int
print(g(1010, 2020))
// CHECK: 6060

// Struct with InOut
let hio = HasInOut(f: { (inout x: Int) in x = 3030 })
i = 0
hio.f(&i)
print(i)
// CHECK: 3030

// Struct that conforms to Protocol with InOut
let hiop = HasInOutProtocol(f: { (inout x: Int) in x = 4040 })
i = 0
hiop.f(&i)
print(i)
// CHECK: 4040

func fooInOut<T>(t: T.Type) -> Any {
  return { (inout x: T) -> () in x = unsafeBitCast((8080, 9090), T.self) }
}

var fio = fooInOut((Int, Int).self)
var gio = fio as! (inout (Int, Int)) -> ()
var xy = (0, 0)
gio(&xy)
print(xy)
// CHECK: (8080, 9090)
