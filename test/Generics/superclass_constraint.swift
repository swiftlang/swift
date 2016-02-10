// RUN: %target-parse-verify-swift

class A {
  func foo() { }
}

class B : A {
  func bar() { }
}

class Other { }

func f1<T : A where T : Other>(_: T) { } // expected-error{{generic parameter 'T' cannot be a subclass of both 'A' and 'Other'}}
func f2<T : A where T : B>(_: T) { }

class GA<T> {}
class GB<T> : GA<T> {}

protocol P {}

func f3<T, U where U : GA<T>>(_: T, _: U) {}
func f4<T, U where U : GA<T>>(_: T, _: U) {}
func f5<T, U : GA<T>>(_: T, _: U) {}
func f6<U : GA<T>, T : P>(_: T, _: U) {}
func f7<U, T where U : GA<T>, T : P>(_: T, _: U) {}

func f8<T : GA<A> where T : GA<B>>(_: T) { } // expected-error{{generic parameter 'T' cannot be a subclass of both 'GA<A>' and 'GA<B>'}}

func f9<T : GA<A> where T : GB<A>>(_: T) { }
func f10<T : GB<A> where T : GA<A>>(_: T) { }

func f11<T : GA<T>>(_: T) { } // expected-error{{superclass constraint 'GA<T>' is recursive}}
func f12<T : GA<U>, U : GB<T>>(_: T, _: U) { } // expected-error{{superclass constraint 'GA<U>' is recursive}}
func f13<T : U, U : GA<T>>(_: T, _: U) { } // expected-error{{inheritance from non-protocol, non-class type 'U'}}
