// RUN: %target-parse-verify-swift

class HasFunc {
  func HasFunc(_: HasFunc) { // expected-error {{use of undeclared type 'HasFunc'}}
  }
  func HasFunc() -> HasFunc { // expected-error {{use of undeclared type 'HasFunc'}}
    return HasFunc()
  }
  func SomethingElse(_: SomethingElse) { // expected-error {{use of undeclared type 'SomethingElse'}}
    return nil
  }
  func SomethingElse() -> SomethingElse? { // expected-error {{use of undeclared type 'SomethingElse'}}
    return nil
  }
}

class HasGenericFunc {
  func HasGenericFunc<HasGenericFunc : HasGenericFunc>(x: HasGenericFunc) -> HasGenericFunc { // expected-error {{inheritance from non-protocol, non-class type 'HasGenericFunc'}}
    return x
  }
  func SomethingElse<SomethingElse : SomethingElse>(_: SomethingElse) -> SomethingElse? { // expected-error {{inheritance from non-protocol, non-class type 'SomethingElse'}}
    return nil
  }
}

class HasProp {
  var HasProp: HasProp { // expected-error 2 {{use of undeclared type 'HasProp'}}
    return HasProp()
  }
  var SomethingElse: SomethingElse? { // expected-error 2 {{use of undeclared type 'SomethingElse'}}
    return nil
  }
}

protocol SomeProtocol {}
protocol ReferenceSomeProtocol {
  var SomeProtocol: SomeProtocol { get } // expected-error 2 {{use of undeclared type 'SomeProtocol'}}
}

func TopLevelFunc(x: TopLevelFunc) -> TopLevelFunc { return x } // expected-error {{use of undeclared type 'TopLevelFunc'}}'
func TopLevelGenericFunc<TopLevelGenericFunc : TopLevelGenericFunc>(x: TopLevelGenericFunc) -> TopLevelGenericFunc { return x } // expected-error {{inheritance from non-protocol, non-class type 'TopLevelGenericFunc'}}
func TopLevelGenericFunc2<T : TopLevelGenericFunc2>(x: T) -> T { return x} // expected-error {{use of undeclared type 'TopLevelGenericFunc2'}}
var TopLevelVar: TopLevelVar? { return nil } // expected-error 2 {{use of undeclared type 'TopLevelVar'}}


protocol AProtocol {
  typealias e : e  // expected-error {{inheritance from non-protocol, non-class type '`Self`.e'}}
}



// <rdar://problem/15604574> Protocol conformance checking needs to be delayed
protocol P15604574 {
  typealias FooResult
  func foo() -> FooResult
}

class AcceptsP<T : P15604574> { }

class X {
  func foo() -> AcceptsP<X> { } // expected-error {{type 'X' does not conform to protocol 'P15604574'}}
}

// <rdar://problem/17144076> recursive typealias causes a segfault in the type checker
struct SomeStruct<A> {
  typealias A = A // expected-error {{type alias 'A' circularly references itself}}
}

