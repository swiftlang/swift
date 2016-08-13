// RUN: %target-parse-verify-swift

class Base {
  func foo() {}
}

class Sub: Base {
  override func foo() {}
}

func removeOverrides<SomeSub: Sub>(concrete: Sub, generic: SomeSub) {
  _ = concrete.foo()
  _ = generic.foo()
}

class Base1 {
	func foo1(a : Int, b : @escaping ()->()) {} // expected-note{{potential overridden instance method 'foo1(a:b:)' here}}
	func foo2(a : @escaping (Int)->(Int), b : @escaping ()->()) {} // expected-note{{potential overridden instance method 'foo2(a:b:)' here}}
}

class Sub1 : Base1 {
	override func foo1(a : Int, b : ()->()) {} // expected-error {{method does not override any method from its superclass}} expected-note {{type does not match superclass instance method with type '(Int, @escaping () -> ()) -> ()'}} {{34-34=@escaping }}
	override func foo2(a : (Int)->(Int), b : ()->()) {} // expected-error {{method does not override any method from its superclass}} expected-note{{type does not match superclass instance method with type '(@escaping (Int) -> (Int), @escaping () -> ()) -> ()'}} {{25-25=@escaping }} {{43-43=@escaping }}
}
