// RUN: %swift -parse -verify %s

class A {
  @objc func do_a() {}

  @objc func do_b(x: Int) {}
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

// Ambiguity between user-defined conversion to optional and the T to
// U? conversion.
class B {
  @conversion func __conversion() -> A? { return .None }
}

class C : A {
  @conversion func __conversion() -> A? { return .None }
}

func test2(b: B, c: C) {
  var aOpt1 : A? = b // okay: user-defined conversion
  var aOpt2 : A? = c // okay: prefers upcast + wrap
}

func test3(b: B?, c: C?) {
  var aOpt1 : A? = c // okay: prefers unwrap + upcast + wrap

  // bad: can't wrap result of user-defined conversion
  var aOpt2 : A? = b // expected-error{{}}
  // okay: wrapped user-defined conversion
  var aOptOpt : A?? = b
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

  takes_optfn(foo)
}

func test5() -> Int? {
  return nil
}

func test6<T>(x : T) {
  // FIXME: this code should work; T could be Int? or Int??
  // or something like that at runtime.  rdar://16374053
  let y = x as? Int? // expected-error {{cannot downcast from 'T' to a more optional type 'Int?'}}
}

func test7(x : A) {
  // This should get diagnosed with the "more optional type" error above.
  let y = x as? B? // expected-error {{'B?' is not a subtype of 'A'}}
}

func test8(x : AnyObject?) {
  let y : A = x as A
}
