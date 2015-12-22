// RUN: %target-parse-verify-swift

func f0(x: Float) -> Float {}
func f1(x: Float) -> Float {}
func f2(@autoclosure x: () -> Float) {}

var f : Float

f0(f0(f))
f0(1)
f1(f1(f))
f2(f)
f2(1.0)

func call_lvalue(@autoclosure rhs: () -> Bool) -> Bool {
  return rhs()
}

// Function returns
func weirdCast<T, U>(x: T) -> U {}

func ff() -> (Int) -> (Float) { return weirdCast }

// Block <-> function conversions

var funct: String -> String = { $0 }
var block: @convention(block) String -> String = funct
funct = block
block = funct

// Application of implicitly unwrapped optional functions

var optFunc: (String -> String)! = { $0 }
var s: String = optFunc("hi")

// <rdar://problem/17652759> Default arguments cause crash with tuple permutation
func testArgumentShuffle(first: Int = 7, third: Int = 9) {
}
testArgumentShuffle(third: 1, 2)



func rejectsAssertStringLiteral() {
  assert("foo") // expected-error {{cannot convert value of type 'String' to expected argument type 'Bool'}}
  precondition("foo") // expected-error {{cannot convert value of type 'String' to expected argument type 'Bool'}}
}



// <rdar://problem/22243469> QoI: Poor error message with throws, default arguments, & overloads
func process(line: UInt = __LINE__, _ fn: () -> Void) {}
func process(line: UInt = __LINE__) -> Int { return 0 }
func dangerous() throws {}

func test() {
  process {         // expected-error {{invalid conversion from throwing function of type '() throws -> ()' to non-throwing function type '() -> Void'}}
    try dangerous()
    test()
  }
}


// <rdar://problem/19962010> QoI: argument label mismatches produce not-great diagnostic
class A {
  func a(text:String) {
  }
  func a(text:String, something:Int?=nil) {
  }
}
A().a(text:"sometext") // expected-error {{argument labels '(text:)' do not match any available overloads}}
// expected-note @-1 {{overloads for 'a' exist with these partially matching parameter lists: (String), (String, something: Int?)}}


// <rdar://problem/22451001> QoI: incorrect diagnostic when argument to print has the wrong type
func r22451001() -> AnyObject {}
print(r22451001(5))  // expected-error {{argument passed to call that takes no arguments}}

