// RUN: %swift -parse -verify %s

class C {
  class func classMethod(Int) {}
  class func classMethod(Int) withArgument(Int) {}
  class func classMethod(Int) withArgument(Int) argument(Int) {}
  class func classMethod(Int) withArgument(Int) argument(Int) argument(_:Int) {}

  func instMethod(Int) {}
  func instMethod(Int) withArgument(Int) {}
  func instMethod(Int) withArgument(Int) argument(Int) {}
  func instMethod(Int) withArgument(Int) argument(Int) argument(_:Int) {}
}

let x = C()

// TODO: Normal method lookup should not find keyword methods
let classM0 = C.classMethod // expected-error{{does not type-check}}
let classM1 = C.classMethod:withArgument: // expected-error{{not implemented}}
let classM2 = C.classMethod:withArgument:argument: // expected-error{{not implemented}}
let classM3 = C.classMethod:withArgument:argument:argument: // expected-error{{not implemented}}

// TODO: recovery
let classMX = C.classMethod: // expected-error{{expected expression}} // expected-error{{does not type-check}} // expected-error{{consecutive statements}}

// TODO: Normal method lookup should not find keyword methods
let instM0 = x.instMethod // expected-error{{does not type-check}}
let instM1 = x.instMethod:withArgument: // expected-error{{not implemented}}
let instM2 = x.instMethod:withArgument:argument: // expected-error{{not implemented}}
let instM3 = x.instMethod:withArgument:argument:argument: // expected-error{{not implemented}}

// TODO: recovery
let instMX = x.instMethod: // expected-error{{expected expression}} // expected-error{{does not type-check}} // expected-error{{consecutive statements}}
