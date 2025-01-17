// RUN: %target-typecheck-verify-swift

protocol P { }

protocol Q {
  associatedtype T
  associatedtype U // expected-note 2{{protocol requires nested type 'U'}}
}

protocol R: Q {
}

struct XT: P { }
struct XTWithoutP { }

struct XU { }

extension R where T: P {
  @_implements(Q, U)
  typealias _Default_U = XU
}

struct Y1: R {
  typealias T = XT
  // okay: infers U = XU
}

struct Y2: R { 
  // expected-error@-1 {{type 'Y2' does not conform to protocol 'Q'}}
  // expected-note@-2 {{add stubs for conformance}}
  typealias T = XTWithoutP

  // FIXME: More detail from diagnostic.
  // error: T: P fails
}

struct Y3: Q { 
  // expected-error@-1 {{type 'Y3' does not conform to protocol 'Q'}}
  // expected-note@-2 {{add stubs for conformance}}
  typealias T = XT
  // FIXME: More detail from diagnostic.
}


protocol A1 {
  associatedtype T
}
protocol A2 {
  associatedtype T
}

struct BothA1_and_A2: A1, A2 {
  @_implements(A1, T)
  typealias X = Int

  @_implements(A2, T)
  typealias Y = String
}

typealias A1_T<U: A1> = U.T
typealias A2_T<U: A2> = U.T

struct RequireSame<T, U> { }

extension RequireSame where T == U {
  init(same: Bool) { }
}

func testImplements() {
  _ = RequireSame<A1_T<BothA1_and_A2>, Int>(same: true)
  _ = RequireSame<A2_T<BothA1_and_A2>, String>(same: true)
}
