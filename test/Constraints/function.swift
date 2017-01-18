// RUN: %target-typecheck-verify-swift

func f0(_ x: Float) -> Float {}
func f1(_ x: Float) -> Float {}
func f2(_ x: @autoclosure () -> Float) {}

var f : Float

_ = f0(f0(f))
_ = f0(1)
_ = f1(f1(f))
f2(f)
f2(1.0)

func call_lvalue(_ rhs: @autoclosure () -> Bool) -> Bool {
  return rhs()
}

// Function returns
func weirdCast<T, U>(_ x: T) -> U {}

func ff() -> (Int) -> (Float) { return weirdCast }

// Block <-> function conversions

var funct: (Int) -> Int = { $0 }
var block: @convention(block) (Int) -> Int = funct
funct = block
block = funct

// Application of implicitly unwrapped optional functions

var optFunc: ((String) -> String)! = { $0 }
var s: String = optFunc("hi")

// <rdar://problem/17652759> Default arguments cause crash with tuple permutation
func testArgumentShuffle(_ first: Int = 7, third: Int = 9) {
}
testArgumentShuffle(third: 1, 2) // expected-error {{unnamed argument #2 must precede argument 'third'}} {{21-29=2}} {{31-32=third: 1}}



func rejectsAssertStringLiteral() {
  assert("foo") // expected-error {{cannot convert value of type 'String' to expected argument type 'Bool'}}
  precondition("foo") // expected-error {{cannot convert value of type 'String' to expected argument type 'Bool'}}
}



// <rdar://problem/22243469> QoI: Poor error message with throws, default arguments, & overloads
func process(_ line: UInt = #line, _ fn: () -> Void) {}
func process(_ line: UInt = #line) -> Int { return 0 }
func dangerous() throws {}

func test() {
  process {         // expected-error {{invalid conversion from throwing function of type '() throws -> ()' to non-throwing function type '() -> Void'}}
    try dangerous()
    test()
  }
}


// <rdar://problem/19962010> QoI: argument label mismatches produce not-great diagnostic
class A {
  func a(_ text:String) {
  }
  func a(_ text:String, something:Int?=nil) {
  }
}
A().a(text:"sometext") // expected-error{{extraneous argument label 'text:' in call}}{{7-12=}}


// <rdar://problem/22451001> QoI: incorrect diagnostic when argument to print has the wrong type
func r22451001() -> AnyObject {}
print(r22451001(5))  // expected-error {{argument passed to call that takes no arguments}}


// SR-590 Passing two parameters to a function that takes one argument of type Any crashes the compiler
// SR-1028: Segmentation Fault: 11 when superclass init takes parameter of type 'Any'
func sr590(_ x: Any) {}  // expected-note {{'sr590' declared here}}
sr590(3,4) // expected-error {{extra argument in call}}
sr590() // expected-error {{missing argument for parameter #1 in call}}
// Make sure calling with structural tuples still works.
sr590(())
sr590((1, 2))

// SR-2657: Poor diagnostics when function arguments should be '@escaping'.
private class SR2657BlockClass<T> {
  let f: T
  init(f: T) { self.f = f }
}

func foo(block: () -> ()) {
  let a = SR2657BlockClass(f: block) // No error
  let b = SR2657BlockClass<()->()>(f: block)
  // expected-error@-1 {{invalid conversion from non-escaping function of type '() -> ()' to potentially escaping function type '() -> ()'}}
  let c: SR2657BlockClass<()->()> = SR2657BlockClass(f: block)
  // expected-error@-1 {{cannot convert value of type 'SR2657BlockClass<() -> ()>' to specified type 'SR2657BlockClass<() -> ()>'}}
  let d: SR2657BlockClass<()->()> = SR2657BlockClass<()->()>(f: block)
  // expected-error@-1 {{invalid conversion from non-escaping function of type '() -> ()' to potentially escaping function type '() -> ()'}}
}
