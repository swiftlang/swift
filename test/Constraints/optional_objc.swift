// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

class A {
  @objc func do_a() {}

  @objc(do_b_2:) func do_b(_ x: Int) {}
  @objc func do_b(_ x: Float) {}

  @objc func do_c(x: Int) {} // expected-note {{incorrect labels for candidate (have: '(_:)', expected: '(x:)')}}
  @objc func do_c(y: Int) {} // expected-note {{incorrect labels for candidate (have: '(_:)', expected: '(y:)')}}
}

func test0(_ a: AnyObject) {
  a.do_a?()

  a.do_b?(1)
  a.do_b?(5.0)

  a.do_c?(1) // expected-error {{no exact matches in call to instance method 'do_c'}}
  a.do_c?(x: 1)
}

func test1(_ a: A) {
  a?.do_a() // expected-error {{cannot use optional chaining on non-optional value of type 'A'}} {{4-5=}}
  a!.do_a() // expected-error {{cannot force unwrap value of non-optional type 'A'}} {{4-5=}}
  // Produce a specialized diagnostic here?
  a.do_a?() // expected-error {{cannot use optional chaining on non-optional value of type '() -> ()'}} {{9-10=}}
}

func test2() {
  func foo() -> Int { return 0 }
  func takes_optfn(_ f : () -> Int?) -> Int? { return f() }

  _ = takes_optfn(foo)

  func takes_objoptfn(_ f : () -> AnyObject?) -> AnyObject? { return f() }
  func objFoo() -> AnyObject { return A() }
  _ = takes_objoptfn(objFoo) // okay
  func objBar() -> A { return A() }
  _ = takes_objoptfn(objBar) // okay
}

// rdar://problem/57668873 - Too eager force optional unwrap fix

@objc class Window {}

@objc protocol WindowDelegate {
  @objc optional var window: Window? { get set }
}

func test_force_unwrap_not_being_too_eager() {
  struct WindowContainer {
    unowned(unsafe) var delegate: WindowDelegate? = nil
  }

  let obj: WindowContainer = WindowContainer()
  if let _ = obj.delegate?.window { // Ok
  }
}
