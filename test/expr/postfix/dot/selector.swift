// RUN: %target-parse-verify-swift

class C {
  class func classMethod(_: Int) {}
  class func classMethod(_: Int, withArgument: Int) {}
  class func classMethod(_: Int, withArgument: Int, argument: Int) {}
  class func classMethod(_: Int, withArgument: Int, argument: Int, argument _:Int) {}

  func instMethod(_: Int) {}
  func instMethod(_: Int, withArgument: Int) {}
  func instMethod(_: Int, withArgument: Int, argument: Int) {}
  func instMethod(_: Int, withArgument: Int, argument: Int, argument _:Int) {}

  func instMethod(_: Int, overloaded: String) {}
  func instMethod(_: Int, overloaded: Int) {}
}

let x = C()

// TODO: Normal method lookup should not find keyword methods
let classM0: (Int) -> () = C.classMethod
let classM1 = C.classMethod:withArgument:
let classM2 = C.classMethod:withArgument:argument:
let classM3 = C.classMethod:withArgument:argument:argument:

// TODO: recovery
let classMX = C.classMethod: // expected-error{{expected expression}} // expected-error{{could not find an overload for 'classMethod' that accepts the supplied arguments}} // expected-error{{consecutive statements}}

// TODO: Normal method lookup should not find keyword methods
let instM0: (Int) -> () = x.instMethod
let instM1 = x.instMethod:withArgument:
let instM2 = x.instMethod:withArgument:argument:
let instM3 = x.instMethod:withArgument:argument:argument:

let instM4: (Int, String) -> () = x.instMethod:overloaded:
let instM5: (Int, Int) -> () = x.instMethod:overloaded:
// ambiguous
let instM6 = x.instMethod:overloaded: // expected-error{{could not find an overload for 'instMethod' that accepts the supplied arguments}}

x.instMethod:overloaded:(1, overloaded: 1)
x.instMethod:overloaded:(1, overloaded: "two")

let instM7 = x.instMethod:nonexistent: // expected-error{{'C' does not have a member named 'instMethod(_:nonexistent:)'}}

// TODO: recovery
let instMX = x.instMethod: // expected-error{{expected expression}} // expected-error{{could not find an overload for 'instMethod' that accepts the supplied arguments}} // expected-error{{consecutive statements}}
