// RUN: %target-parse-verify-swift

protocol SomeProtocol {
	associatedtype T
}

extension SomeProtocol where T == Optional<T> { } // expected-error{{same-type constraint 'Self.T' == 'Optional<Self.T>' is recursive}}

// rdar://problem/19840527
class X<T> where T == X { // expected-error{{same-type requirement makes generic parameter 'T' non-generic}}
    var type: T { return type(of: self) } // expected-error{{cannot convert return expression of type 'X<T>.Type' to return type 'T'}}
}

protocol Y {
  associatedtype Z = Z // expected-error{{type alias 'Z' circularly references itself}}
}

// rdar://problem/20000145

public protocol P {
  associatedtype T
}
public struct S<A: P> where A.T == S<A> {} // expected-error{{type may not reference itself as a requirement}}

protocol I {
  init()
}

protocol PI {
  associatedtype T : I
}

struct SI<A: PI> : I where A : I, A.T == SI<A> { // expected-error{{type may not reference itself as a requirement}}
}

/* FIXME: Infinite recursion

struct S4<A: PI> : I where A : I {
}

struct S5<A: PI> : I where A : I, A.T == S4<A> {
}*/

/* FIXME: Hits ArchetypeBuilder assertions

struct SU<A: P> where A.T == SU {
}

struct SIU<A: PI> : I where A : I, A.T == SIU {
}*/
