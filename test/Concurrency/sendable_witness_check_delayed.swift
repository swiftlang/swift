// RUN: %target-swift-frontend -o /dev/null -emit-sil %s -verify
// RUN: %target-swift-frontend -o /dev/null -emit-sil %s -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -o /dev/null -emit-sil %s -verify -strict-concurrency=complete
// RUN: %target-swift-frontend -o /dev/null -emit-sil %s -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: swift_feature_RegionBasedIsolation

// This triggers a conformance check with SuppressDiagnostics=true.
let x = S().f {}

protocol P {
  associatedtype A

  func f(_: A) -> Int // expected-note {{expected sendability to match requirement here}}
}

struct S : P {
  typealias A = () -> ()
  func f(_: @Sendable () -> ()) -> Int { return 0 }
  // expected-warning@-1 {{sendability of function types in instance method 'f' does not match requirement in protocol 'P'}}
}
