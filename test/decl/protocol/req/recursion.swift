// RUN: %target-typecheck-verify-swift

protocol SomeProtocol {
	associatedtype T
}

extension SomeProtocol where T == Optional<T> { }
// expected-error@-1 {{cannot build rewrite system for generic signature; concrete type nesting limit exceeded}}
// expected-note@-2 {{failed rewrite rule is τ_0_0.[SomeProtocol:T].[concrete: Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<Optional<τ_0_0.[SomeProtocol:T]>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>] => τ_0_0.[SomeProtocol:T]}}

// rdar://problem/19840527

class X<T> where T == X {
// expected-error@-1 {{cannot build rewrite system for generic signature; concrete type nesting limit exceeded}}
// expected-note@-2 {{failed rewrite rule is τ_0_0.[concrete: X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<X<τ_0_0>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>] => τ_0_0}}
// expected-error@-3 {{generic class 'X' has self-referential generic requirements}}
    var type: T { return Swift.type(of: self) } // expected-error{{cannot convert return expression of type 'X<T>.Type' to return type 'T'}}
}

// FIXME: The "associated type 'Foo' is not a member type of 'Self'" diagnostic
// should also become "associated type 'Foo' references itself"
protocol CircularAssocTypeDefault {
  associatedtype Z = Z // expected-error{{associated type 'Z' references itself}}
  // expected-note@-1{{type declared here}}
  // expected-note@-2{{protocol requires nested type 'Z'}}

  associatedtype Z2 = Z3
  // expected-note@-1{{protocol requires nested type 'Z2'}}
  associatedtype Z3 = Z2
  // expected-note@-1{{protocol requires nested type 'Z3'}}

  associatedtype Z4 = Self.Z4 // expected-error{{associated type 'Z4' references itself}}
  // expected-note@-1{{type declared here}}
  // expected-note@-2{{protocol requires nested type 'Z4'}}

  associatedtype Z5 = Self.Z6
  // expected-note@-1{{protocol requires nested type 'Z5'}}
  associatedtype Z6 = Self.Z5
  // expected-note@-1{{protocol requires nested type 'Z6'}}
}

struct ConformsToCircularAssocTypeDefault : CircularAssocTypeDefault { }
// expected-error@-1 {{type 'ConformsToCircularAssocTypeDefault' does not conform to protocol 'CircularAssocTypeDefault'}}

// rdar://problem/20000145
public protocol P {
  associatedtype T
}

public struct S<A: P> where A.T == S<A> {
// expected-error@-1 {{generic struct 'S' has self-referential generic requirements}}
  func f(a: A.T) {
    g(a: id(t: a)) // `a` has error type which is diagnosed as circular reference
    _ = A.T.self
  }

  func g(a: S<A>) {
    f(a: id(t: a))
    _ = S<A>.self
  }

  func id<T>(t: T) -> T {
    return t
  }
}

protocol I {
  init()
}

protocol PI {
  associatedtype T : I
}

struct SI<A: PI> : I where A : I, A.T == SI<A> {
// expected-error@-1 {{generic struct 'SI' has self-referential generic requirements}}
  func ggg<T : I>(t: T.Type) -> T {
    return T()
  }

  func foo() {
    _ = A()

    _ = A.T()
    _ = SI<A>()

    _ = ggg(t: A.self)
    _ = ggg(t: A.T.self)

    _ = self.ggg(t: A.self)
    _ = self.ggg(t: A.T.self)
  }
}

// Used to hit infinite recursion
struct S4<A: PI> : I where A : I {
}

struct S5<A: PI> : I where A : I, A.T == S4<A> { }

// Used to hit ArchetypeBuilder assertions
struct SU<A: P> where A.T == SU {
// expected-error@-1 {{generic struct 'SU' has self-referential generic requirements}}
}

struct SIU<A: PI> : I where A : I, A.T == SIU {
// expected-error@-1 {{generic struct 'SIU' has self-referential generic requirements}}
}
