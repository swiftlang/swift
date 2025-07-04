// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-upcoming-feature NonisolatedNonsendingByDefault -emit-module-path %t/WithFeature.swiftmodule -module-name WithFeature %S/Inputs/caller_inheriting_isolation.swift -swift-version 6
// RUN: %target-swift-frontend -emit-module-path %t/WithoutFeature.swiftmodule -module-name WithoutFeature %S/Inputs/caller_inheriting_isolation.swift -swift-version 6

// RUN: %target-swift-frontend -module-name main -I %t %s -emit-sil -o - | %FileCheck %s

// RUN: %target-swift-frontend -enable-upcoming-feature NonisolatedNonsendingByDefault -module-name main -I %t %s -emit-sil -verify -swift-version 6

// REQUIRES: asserts
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

import WithFeature
import WithoutFeature

class NonSendable {}

actor A {
  var ns = NonSendable()

  // CHECK-LABEL: // unspecifiedAsync<A>(_:)
  // CHECK-NEXT: // Isolation: caller_isolation_inheriting
  // CHECK-NEXT: sil @$s11WithFeature16unspecifiedAsyncyyxYalF : $@convention(thin) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
  func test1() async {
    // If unspecifiedAsync does not inherit the isolation of A, then we will get
    // an error.
    await WithFeature.unspecifiedAsync(ns)
  }

  // CHECK-LABEL: // unspecifiedAsyncConcurrent<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s11WithFeature26unspecifiedAsyncConcurrentyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
  func test1a() async {
    await WithFeature.unspecifiedAsyncConcurrent(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent global function 'unspecifiedAsyncConcurrent' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // unspecifiedAsyncCaller<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s11WithFeature22unspecifiedAsyncCalleryyxYalF : $@convention(thin) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
  func test1b() async {
    await WithFeature.unspecifiedAsyncCaller(ns)
  }

  // CHECK-LABEL: // unspecifiedAsync<A>(_:)
  // CHECK: // Isolation: unspecified
  // CHECK: sil @$s14WithoutFeature16unspecifiedAsyncyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
  func test2() async {
    await WithoutFeature.unspecifiedAsync(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent global function 'unspecifiedAsync' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // unspecifiedAsyncConcurrent<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s14WithoutFeature26unspecifiedAsyncConcurrentyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
  func test2a() async {
    // If unspecifiedAsync does not inherit the isolation of A, then we will get
    // an error.
    await WithoutFeature.unspecifiedAsyncConcurrent(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent global function 'unspecifiedAsyncConcurrent' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // unspecifiedAsyncCaller<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s14WithoutFeature22unspecifiedAsyncCalleryyxYalF : $@convention(thin) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
  func test2b() async {
    await WithoutFeature.unspecifiedAsyncCaller(ns)
  }

  // CHECK-LABEL: // nonisolatedAsync<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s11WithFeature16nonisolatedAsyncyyxYalF : $@convention(thin) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
  func test3() async {
    await WithFeature.nonisolatedAsync(ns)
  }

  // CHECK-LABEL: // nonisolatedAsyncConcurrent<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s11WithFeature26nonisolatedAsyncConcurrentyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
  func test3a() async {
    await WithFeature.nonisolatedAsyncConcurrent(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent global function 'nonisolatedAsyncConcurrent' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // nonisolatedAsyncCaller<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s11WithFeature22nonisolatedAsyncCalleryyxYalF : $@convention(thin) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
  func test3b() async {
    await WithFeature.nonisolatedAsyncCaller(ns)
  }

  // CHECK-LABEL: // nonisolatedAsync<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s14WithoutFeature16nonisolatedAsyncyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
  func test4() async {
    await WithoutFeature.nonisolatedAsync(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent global function 'nonisolatedAsync' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // nonisolatedAsyncConcurrent<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s14WithoutFeature26nonisolatedAsyncConcurrentyyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
  func test4a() async {
    await WithoutFeature.nonisolatedAsyncConcurrent(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent global function 'nonisolatedAsyncConcurrent' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // nonisolatedAsyncCaller<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s14WithoutFeature22nonisolatedAsyncCalleryyxYalF : $@convention(thin) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
  func test4b() async {
    await WithoutFeature.nonisolatedAsyncCaller(ns)
  }

  // CHECK-LABEL: // S.unspecifiedAsync<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s11WithFeature1SV16unspecifiedAsyncyyxYalF : $@convention(method) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0, S) -> ()
  func test5() async {
    let s = WithFeature.S()
    await s.unspecifiedAsync(ns)
  }

  // CHECK-LABEL: // S.unspecifiedAsyncConcurrent<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s11WithFeature1SV26unspecifiedAsyncConcurrentyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()
  func test5a() async {
    let s = WithFeature.S()
    await s.unspecifiedAsyncConcurrent(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent instance method 'unspecifiedAsyncConcurrent' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // S.unspecifiedAsyncCaller<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s11WithFeature1SV22unspecifiedAsyncCalleryyxYalF : $@convention(method) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0, S) -> ()
  func test5b() async {
    let s = WithFeature.S()
    await s.unspecifiedAsyncCaller(ns)
  }

  // CHECK-LABEL: // S.nonisolatedAsync<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s11WithFeature1SV16nonisolatedAsyncyyxYalF : $@convention(method) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0, S) -> ()
  func test6() async {
    let s = WithFeature.S()
    await s.nonisolatedAsync(ns)
  }

  // CHECK-LABEL: // S.nonisolatedAsyncConcurrent<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s11WithFeature1SV26nonisolatedAsyncConcurrentyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()
  func test6a() async {
    let s = WithFeature.S()
    await s.nonisolatedAsyncConcurrent(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent instance method 'nonisolatedAsyncConcurrent' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // S.nonisolatedAsyncCaller<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s11WithFeature1SV22nonisolatedAsyncCalleryyxYalF : $@convention(method) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0, S) -> ()
  func test6b() async {
    let s = WithFeature.S()
    await s.nonisolatedAsyncCaller(ns)
  }

  // CHECK-LABEL: // S.unspecifiedAsync<A>(_:)
  // CHECK: // Isolation: unspecified
  // CHECK: sil @$s14WithoutFeature1SV16unspecifiedAsyncyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()
  func test7() async {
    let s = WithoutFeature.S()
    await s.unspecifiedAsync(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent instance method 'unspecifiedAsync' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // S.unspecifiedAsyncConcurrent<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s14WithoutFeature1SV26unspecifiedAsyncConcurrentyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()
  func test7a() async {
    let s = WithoutFeature.S()
    await s.unspecifiedAsyncConcurrent(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent instance method 'unspecifiedAsyncConcurrent' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // S.unspecifiedAsyncCaller<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s14WithoutFeature1SV22unspecifiedAsyncCalleryyxYalF : $@convention(method) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0, S) -> ()
  func test7b() async {
    let s = WithoutFeature.S()
    await s.unspecifiedAsyncCaller(ns)
  }

  // CHECK-LABEL: // S.nonisolatedAsync<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s14WithoutFeature1SV16nonisolatedAsyncyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()
  func test8() async {
    let s = WithoutFeature.S()
    await s.nonisolatedAsync(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent instance method 'nonisolatedAsync' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // S.nonisolatedAsyncConcurrent<A>(_:)
  // CHECK: // Isolation: nonisolated
  // CHECK: sil @$s14WithoutFeature1SV26nonisolatedAsyncConcurrentyyxYalF : $@convention(method) @async <τ_0_0> (@in_guaranteed τ_0_0, S) -> ()
  func test8a() async {
    let s = WithoutFeature.S()
    await s.nonisolatedAsyncConcurrent(ns)
    // expected-error @-1 {{sending 'self.ns' risks causing data races}}
    // expected-note @-2 {{sending 'self'-isolated 'self.ns' to @concurrent instance method 'nonisolatedAsyncConcurrent' risks causing data races between @concurrent and 'self'-isolated uses}}
  }

  // CHECK-LABEL: // S.nonisolatedAsyncCaller<A>(_:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil @$s14WithoutFeature1SV22nonisolatedAsyncCalleryyxYalF : $@convention(method) @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0, S) -> ()
  func test8b() async {
    let s = WithoutFeature.S()
    await s.nonisolatedAsyncCaller(ns)
  }
}
