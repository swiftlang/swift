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
