// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

class A {
  @objc func do_a() {}

  @objc(do_b_2:) func do_b(x: Int) {}
  @objc func do_b(x: Float) {}

  @objc func do_c(#x: Int) {}
  @objc func do_c(#y: Int) {}
}

func test0(a: AnyObject) {
  a.do_a?()

  a.do_b?(1)
  a.do_b?(5.0)

  a.do_c?(1) // expected-error {{'AnyObject' does not have a member named 'do_c'}}
  a.do_c?(x: 1)
}

func test1(a: A) {
  a?.do_a() // expected-error {{operand of postfix '?' should have optional type; type is 'A'}} {{4-5=}}
  a!.do_a() // expected-error {{operand of postfix '!' should have optional type; type is 'A'}} {{4-5=}}
  // Produce a specialized diagnostic here?
  a.do_a?() // expected-error {{operand of postfix '?' should have optional type; type is '() -> ()'}} {{9-10=}}
}

// <rdar://problem/15508756>
extension Optional {
  func bind<U>(f: T -> U?) -> U? {
    switch self {
    case .Some(var x):
      return f(x)
    case .None:
      return .None
    }
  }
}

var c: String? = Optional<Int>(1)
  .bind {(x: Int) in println("\(x)!"); return "two" }

func test4() {
  func foo() -> Int { return 0 }
  func takes_optfn(f : () -> Int?) -> Int? { return f() }

  takes_optfn(foo) // expected-error {{function signature '() -> Int' is not compatible with expected type '() -> Int?'}} expected-note {{use a closure to safely wrap calls to the function}}

  func takes_objoptfn(f : () -> AnyObject?) -> AnyObject? { return f() }
  func objFoo() -> AnyObject { return A() }
  takes_objoptfn(objFoo) // okay
  func objBar() -> A { return A() }
  takes_objoptfn(objBar) // okay
}

func test5() -> Int? {
  return nil
}

func test6<T>(x : T) {
  // FIXME: this code should work; T could be Int? or Int??
  // or something like that at runtime.  rdar://16374053
  let y = x as? Int? // expected-error {{cannot downcast from 'T' to a more optional type 'Int?'}}
}

class B : A { }

func test7(x : A) {
  let y = x as? B? // expected-error{{cannot downcast from 'A' to a more optional type 'B?'}}
}

func test8(x : AnyObject?) {
  let y : A = x as! A
}


// Partial ordering with optionals
func test9_helper<T>(x: T) -> Int { }
func test9_helper<T>(x: T?) -> Double { }

func test9(i: Int, io: Int?) {
  var result = test9_helper(i)
  var i2: Int = result
  var result2 = test9_helper(io)
  let d: Double = result2
}

protocol P { }

func test10_helper<T : P>(x: T) -> Int { }
func test10_helper<T : P>(x: T?) -> Double { }

extension Int : P { }

func test10(i: Int, io: Int?) {
  let result = test10_helper(i)
  var i2: Int = result

  let result2 = test10_helper(io)
  var d: Double = result2
}

var z: Int? = nil
z = z ?? 3

var fo: Float? = 3.14159
