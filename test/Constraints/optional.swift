// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

func markUsed<T>(_ t: T) {}

class A {
  @objc func do_a() {}

  @objc(do_b_2:) func do_b(_ x: Int) {}
  @objc func do_b(_ x: Float) {}

  @objc func do_c(x: Int) {}
  @objc func do_c(y: Int) {}
}

func test0(_ a: AnyObject) {
  a.do_a?()

  a.do_b?(1)
  a.do_b?(5.0)

  a.do_c?(1) // expected-error {{cannot invoke value of function type with argument list '(Int)'}}
  a.do_c?(x: 1)
}

func test1(_ a: A) {
  a?.do_a() // expected-error {{cannot use optional chaining on non-optional value of type 'A'}} {{4-5=}}
  a!.do_a() // expected-error {{cannot force unwrap value of non-optional type 'A'}} {{4-5=}}
  // Produce a specialized diagnostic here?
  a.do_a?() // expected-error {{cannot use optional chaining on non-optional value of type '() -> ()'}} {{9-10=}}
}

// <rdar://problem/15508756>
extension Optional {
  func bind<U>(_ f: (Wrapped) -> U?) -> U? {
    switch self {
    case .some(let x):
      return f(x)
    case .none:
      return .none
    }
  }
}

var c: String? = Optional<Int>(1)
  .bind {(x: Int) in markUsed("\(x)!"); return "two" }

func test4() {
  func foo() -> Int { return 0 }
  func takes_optfn(_ f : () -> Int?) -> Int? { return f() }

  _ = takes_optfn(foo)

  func takes_objoptfn(_ f : () -> AnyObject?) -> AnyObject? { return f() }
  func objFoo() -> AnyObject { return A() }
  _ = takes_objoptfn(objFoo) // okay
  func objBar() -> A { return A() }
  _ = takes_objoptfn(objBar) // okay
}

func test5() -> Int? {
  return nil
}

func test6<T>(_ x : T) {
  // FIXME: this code should work; T could be Int? or Int??
  // or something like that at runtime.  rdar://16374053
  _ = x as? Int? // expected-error {{cannot downcast from 'T' to a more optional type 'Int?'}}
}

class B : A { }

func test7(_ x : A) {
  _ = x as? B? // expected-error{{cannot downcast from 'A' to a more optional type 'B?'}}
}

func test8(_ x : AnyObject?) {
  let _ : A = x as! A
}


// Partial ordering with optionals
func test9_helper<T>(_ x: T) -> Int { }
func test9_helper<T>(_ x: T?) -> Double { }

func test9(_ i: Int, io: Int?) {
  let result = test9_helper(i)
  var _: Int = result
  let result2 = test9_helper(io)
  let _: Double = result2
}

protocol P { }

func test10_helper<T : P>(_ x: T) -> Int { }
func test10_helper<T : P>(_ x: T?) -> Double { }

extension Int : P { }

func test10(_ i: Int, io: Int?) {
  let result = test10_helper(i)
  var _: Int = result

  let result2 = test10_helper(io)
  var _: Double = result2
}

var z: Int? = nil
z = z ?? 3

var fo: Float? = 3.14159

func voidOptional(_ handler: () -> ()?) {}
func testVoidOptional() {
  let noop: () -> Void = {}
  voidOptional(noop)

  let optNoop: (()?) -> ()? = { return $0 }
  voidOptional(optNoop)
}

func testTernaryWithNil(b: Bool, s: String, i: Int) {
  let t1 = b ? s : nil
  let _: Double = t1 // expected-error{{value of type 'String?'}}
  let t2 = b ? nil : i
  let _: Double = t2 // expected-error{{value of type 'Int?'}}
  let t3 = b ? "hello" : nil
  let _: Double = t3 // expected-error{{value of type 'String?'}}
  let t4 = b ? nil : 1
  let _: Double = t4 // expected-error{{value of type 'Int?'}}
}

// inference with IUOs
infix operator ++++

protocol PPPP {
  static func ++++(x: Self, y: Self) -> Bool
}

func compare<T: PPPP>(v: T, u: T!) -> Bool {
  return v ++++ u
}

func sr2752(x: String?, y: String?) {
  _ = x.map { xx in
    y.map { _ in "" } ?? "\(xx)"
  }
}

// SR-3248 - Invalid diagnostic calling implicitly unwrapped closure
var sr3248 : ((Int) -> ())!
sr3248?(a: 2) // expected-error {{extraneous argument label 'a:' in call}}
sr3248!(a: 2) // expected-error {{extraneous argument label 'a:' in call}}
sr3248(a: 2)  // expected-error {{extraneous argument label 'a:' in call}}

struct SR_3248 {
    var callback: (([AnyObject]) -> Void)!
}

SR_3248().callback?("test") // expected-error {{cannot convert value of type 'String' to expected argument type '[AnyObject]'}}
SR_3248().callback!("test") // expected-error {{cannot convert value of type 'String' to expected argument type '[AnyObject]'}}
SR_3248().callback("test")  // expected-error {{cannot convert value of type 'String' to expected argument type '[AnyObject]'}}

_? = nil  // expected-error {{'nil' requires a contextual type}}
_?? = nil // expected-error {{'nil' requires a contextual type}}


// rdar://problem/29993596
func takeAnyObjects(_ lhs: AnyObject?, _ rhs: AnyObject?) { }

infix operator !====

func !====(_ lhs: AnyObject?, _ rhs: AnyObject?) -> Bool { return false }

func testAnyObjectImplicitForce(lhs: AnyObject?!, rhs: AnyObject?) {
  if lhs !==== rhs { }

  takeAnyObjects(lhs, rhs)
}
