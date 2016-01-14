// RUN: %target-parse-verify-swift

protocol SomeProtocol {
	typealias T
}

extension SomeProtocol where T == Optional<T> { } // expected-error{{same-type constraint 'Self.T' == 'Optional<Self.T>' is recursive}}

// rdar://problem/20000145
public protocol P {
  typealias T
}
public struct S<A: P where A.T == S<A>> {}

// rdar://problem/19840527
class X<T where T == X> { // expected-error{{same-type requirement makes generic parameter 'T' non-generic}}
    var type: T { return self.dynamicType } // expected-error{{cannot convert return expression of type 'X<T>.Type' to return type 'T'}}
}

protocol Y {
  typealias Z = Z // expected-error{{type alias 'Z' circularly references itself}}
}
