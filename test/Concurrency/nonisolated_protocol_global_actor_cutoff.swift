// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %s -emit-sil -o /dev/null -verify -enable-experimental-feature NoExplicitNonIsolated

// REQUIRES: concurrency
// REQUIRES: swift_feature_NoExplicitNonIsolated

@MainActor
func onMain() {}

@MainActor
protocol IsolatedBase {}

@MainActor
// expected-note@+1{{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
class Req {}

protocol IsolatedRequirements: IsolatedBase {
  var req: Req { get }
}

nonisolated protocol NonisolatedRequirements: IsolatedBase {
  var req: Req { get }
}

nonisolated protocol NonisolatedRefinement: IsolatedBase {}

protocol PlainBase {}
nonisolated protocol PlainRefinement: PlainBase {}

struct A: NonisolatedRefinement { func f() {} }

struct B: PlainRefinement { func f() {} }

struct C { func f() {} }
extension C: NonisolatedRefinement {}

protocol IndirectRefinement: NonisolatedRefinement {}

struct D: IndirectRefinement { func f() {} }

nonisolated func probe(a: A, b: B, c: C, d: D) {
  a.f()
  b.f()
  c.f()
  d.f()
}

@MainActor protocol IsolatedRefinement: NonisolatedRefinement {}

struct E: IsolatedRefinement {
  func h() { onMain() }
}

struct F: NonisolatedRefinement, IsolatedBase {
  func k() { onMain() }
}

struct S: IsolatedRequirements {
  var req = Req()
}

struct S2: NonisolatedRequirements {
  // expected-error@+1{{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}
  var req = Req()
}

// Witnesses of an explicitly 'nonisolated' type don't take the isolation of
// the requirements they satisfy.
@MainActor
protocol IsolatedMethodRequirement {
  func req()
}

nonisolated struct G: IsolatedMethodRequirement {
  func req() {}
}

protocol MainActorMethodRequirement {
  @MainActor func req()
}

nonisolated struct H: MainActorMethodRequirement {
  func req() {}
}

struct I: MainActorMethodRequirement {
  // expected-note@+2{{calls to instance method 'req()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@+1{{main actor isolation inferred from conformance to protocol 'MainActorMethodRequirement'}}
  func req() {}
}

nonisolated func probeNonisolatedTypes(g: G, h: H, i: I) {
  g.req()
  h.req()
  // expected-error@+1{{call to main actor-isolated instance method 'req()' in a synchronous nonisolated context}}
  i.req()
}
