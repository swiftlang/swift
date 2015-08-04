// RUN: %target-parse-verify-swift

class C {
  class func classMethod(_: Int) {} // expected-note {{found this candidate}}
  class func classMethod(_: Int, withArgument: Int) {} // expected-note {{found this candidate}}
  class func classMethod(_: Int, withArgument: Int, argument: Int) {} // expected-note {{found this candidate}}
  class func classMethod(_: Int, withArgument: Int, argument: Int, argument _:Int) {} // expected-note {{found this candidate}}

  func instMethod(_: Int) {} // expected-note {{found this candidate}}
  func instMethod(_: Int, withArgument: Int) {} // expected-note {{found this candidate}}
  func instMethod(_: Int, withArgument: Int, argument: Int) {} // expected-note {{found this candidate}}
  func instMethod(_: Int, withArgument: Int, argument: Int, argument _:Int) {} // expected-note {{found this candidate}}

  func instMethod(_: Int, overloaded: String) {} // expected-note 2 {{found this candidate}}
  func instMethod(_: Int, overloaded: Int) {}    // expected-note 2 {{found this candidate}}
}

let x = C()

// TODO: Normal method lookup should not find keyword methods
let classM0: (Int) -> () = C.classMethod
let classM1 = C.classMethod:withArgument:
let classM2 = C.classMethod:withArgument:argument:
let classM3 = C.classMethod:withArgument:argument:argument:

// TODO: recovery
let classMX = C.classMethod: // expected-error{{expected expression}} // expected-error{{ambiguous use of 'classMethod'}} // expected-error{{consecutive statements}} {{28-28=;}}

// TODO: Normal method lookup should not find keyword methods
let instM0: (Int) -> () = x.instMethod
let instM1 = x.instMethod:withArgument:
let instM2 = x.instMethod:withArgument:argument:
let instM3 = x.instMethod:withArgument:argument:argument:

let instM4: (Int, String) -> () = x.instMethod:overloaded:
let instM5: (Int, Int) -> () = x.instMethod:overloaded:
// ambiguous
let instM6 = x.instMethod:overloaded: // expected-error{{ambiguous use of 'instMethod(_:overloaded:)'}}

x.instMethod:overloaded:(1, overloaded: 1)
x.instMethod:overloaded:(1, overloaded: "two")

let instM7 = x.instMethod:nonexistent: // expected-error{{value of type 'C' has no member 'instMethod(_:nonexistent:)'}}

// TODO: recovery
let instMX = x.instMethod: // expected-error{{expected expression}} // expected-error{{ambiguous use of 'instMethod'}} // expected-error{{consecutive statements}} {{26-26=;}}
