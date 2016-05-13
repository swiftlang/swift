// RUN: %target-parse-verify-swift

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
  let y = x as? Int? // expected-error {{cannot downcast from 'T' to a more optional type 'Int?'}}
}

class B : A { }

func test7(_ x : A) {
  let y = x as? B? // expected-error{{cannot downcast from 'A' to a more optional type 'B?'}}
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
