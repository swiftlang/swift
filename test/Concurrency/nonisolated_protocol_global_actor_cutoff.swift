// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %s -emit-sil -o /dev/null -verify -language-mode 6
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %s -emit-sil -o /dev/null -verify -language-mode 6 -enable-experimental-feature NoExplicitNonIsolated

// REQUIRES: concurrency
// REQUIRES: swift_feature_NoExplicitNonIsolated

@MainActor
func onMain() {}

@MainActor
protocol IsolatedBase {}

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
