// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature NonIsolatedAsyncInheritsIsolationFromContext -emit-module-path %t/WithFeature.swiftmodule -module-name WithFeature %S/Inputs/caller_inheriting_isolation.swift -swift-version 6
// RUN: %target-swift-frontend -emit-module-path %t/WithoutFeature.swiftmodule -module-name WithoutFeature %S/Inputs/caller_inheriting_isolation.swift -swift-version 6

// RUN: %target-swift-frontend -module-name main -I %t %s -emit-sil -o - | %FileCheck %s

// RUN: %target-swift-frontend -enable-experimental-feature NonIsolatedAsyncInheritsIsolationFromContext -module-name main -I %t %s -emit-sil -verify -swift-version 6

// REQUIRES: asserts
// REQUIRES: swift_feature_NonIsolatedAsyncInheritsIsolationFromContext

import WithFeature
import WithoutFeature

class NonSendable {}

// CHECK-LABEL: // unspecifiedAsync<A>(_:)
// CHECK-NEXT: // Isolation: nonisolated
// CHECK-NEXT: sil @$s11WithFeature16unspecifiedAsyncyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()

// CHECK-LABEL: // unspecifiedAsync<A>(_:)
// CHECK: // Isolation: concurrent
// CHECK: sil @$s14WithoutFeature16unspecifiedAsyncyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()

// CHECK-LABEL: // nonisolatedAsync<A>(_:)
// CHECK: // Isolation: nonisolated
// CHECK: sil @$s11WithFeature16nonisolatedAsyncyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()

// CHECK-LABEL: // nonisolatedAsync<A>(_:)
// CHECK: // Isolation: concurrent
// CHECK: sil @$s14WithoutFeature16nonisolatedAsyncyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()

// CHECK-LABEL: // S.unspecifiedAsync<A>(_:)
// CHECK: // Isolation: nonisolated
// CHECK: sil @$s11WithFeature1SV16unspecifiedAsyncyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()

// CHECK-LABEL: // S.nonisolatedAsync<A>(_:)
// CHECK: // Isolation: nonisolated
// CHECK: sil @$s11WithFeature1SV16nonisolatedAsyncyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()

// CHECK-LABEL: // S.unspecifiedAsync<A>(_:)
// CHECK: // Isolation: concurrent
// CHECK: sil @$s14WithoutFeature1SV16unspecifiedAsyncyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()

// CHECK-LABEL: // S.nonisolatedAsync<A>(_:)
// CHECK: // Isolation: concurrent
// CHECK: sil @$s14WithoutFeature1SV16nonisolatedAsyncyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()

actor A {
  var ns = NonSendable()

  func test() async {
    // If unspecifiedAsync does not inherit the isolation of A, then we will get
    // an error.
    await WithFeature.unspecifiedAsync(ns)
  }

  func test2() async {
    // If unspecifiedAsync does not inherit the isolation of A, then we will get
    // an error.
    await WithoutFeature.unspecifiedAsync(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to nonisolated global function 'unspecifiedAsync' risks causing data races between nonisolated and 'self'-isolated uses}}
  }

  func test3() async {
    // If nonisolatedAsync does not inherit the isolation of A, then we will get
    // an error.
    await WithFeature.nonisolatedAsync(ns)
  }

  func test4() async {
    // If nonisolatedAsync does not inherit the isolation of A, then we will get
    // an error.
    await WithoutFeature.nonisolatedAsync(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to nonisolated global function 'nonisolatedAsync' risks causing data races between nonisolated and 'self'-isolated uses}}
  }

  func test5() async {
    let s = WithFeature.S()
    await s.unspecifiedAsync(ns)
  }

  func test6() async {
    let s = WithFeature.S()
    await s.nonisolatedAsync(ns)
  }

  func test7() async {
    let s = WithoutFeature.S()
    await s.unspecifiedAsync(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to nonisolated instance method 'unspecifiedAsync' risks causing data races between nonisolated and 'self'-isolated uses}}
  }

  func test8() async {
    let s = WithoutFeature.S()
    await s.nonisolatedAsync(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to nonisolated instance method 'nonisolatedAsync' risks causing data races between nonisolated and 'self'-isolated uses}}
  }
}
