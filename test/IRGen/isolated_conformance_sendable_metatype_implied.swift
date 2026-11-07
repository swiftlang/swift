// RUN: %target-swift-frontend -primary-file %s -emit-ir -target %target-swift-5.1-abi-triple -swift-version 6 -enable-upcoming-feature InferIsolatedConformances | %FileCheck %s

// REQUIRES: PTRSIZE=64
// REQUIRES: concurrency
// REQUIRES: swift_feature_InferIsolatedConformances
// UNSUPPORTED: CPU=arm64e

// A conformance implied by a conformance to a protocol that inherits 'Sendable'
// is emitted without the global actor isolation flag (1 << 19 = 524288).

protocol P {
  func f() async
}

protocol SendableP: P, Sendable {}

// CHECK-DAG: @"$s46isolated_conformance_sendable_metatype_implied8CImpliedCAA1PAAMc" = {{.*}}constant %swift.protocol_conformance_descriptor {{.*}}, i32 0 }
@MainActor
final class CImplied: SendableP {
  func f() async {}
}

// Nothing in the chain inherits 'Sendable', so both conformances are inferred
// to be isolated.
protocol Q {
  func g()
}

protocol RefinesQ: Q {}

// CHECK-DAG: @"$s46isolated_conformance_sendable_metatype_implied11CNotImpliedCAA1QAAMc" = {{.*}}constant { i32, i32, i32, i32, i32, i32 } {{.*}}, i32 524288, {{.*}}ScM
// CHECK-DAG: @"$s46isolated_conformance_sendable_metatype_implied11CNotImpliedCAA8RefinesQAAMc" = {{.*}}constant { i32, i32, i32, i32, i32, i32 } {{.*}}, i32 524288, {{.*}}ScM
@MainActor
final class CNotImplied: RefinesQ {
  func g() {}
}
