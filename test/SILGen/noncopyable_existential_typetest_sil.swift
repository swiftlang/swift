// RUN: %target-swift-emit-silgen %s -target %target-future-triple -enable-experimental-feature NoncopyableCasting | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting

// The point of `checked_cast_addr_br test_only` is what it *doesn't* emit. The
// ordinary lowering answers a cast question by materialising the payload and
// throwing it away -- a copy the type forbids, or a take that destroys the value
// being asked about. So these tests are mostly CHECK-NOT: no copy of the
// container, no scratch buffer, no destroy, and no consuming cast.

protocol P: ~Copyable {}

struct Small: ~Copyable, P { var tag: Int }

struct Big: ~Copyable, P {
  var tag: Int
  var pad0, pad1, pad2, pad3, pad4, pad5, pad6: Int
}

struct Unrelated: ~Copyable, P {}

// MARK: - Expression `is` on a borrowed subject

// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil11isBorrowing{{[_0-9a-zA-Z]*}}F
// CHECK:           checked_cast_addr_br test_only any P & ~Copyable in {{%.*}} to Big
// CHECK-NOT:       copy_addr
// CHECK-NOT:       destroy_addr
// CHECK-NOT:       alloc_stack $Big
// CHECK:         } // end sil function
func isBorrowing(_ box: borrowing any P & ~Copyable) -> Bool {
  box is Big
}

// Two tests against the same borrowed subject: neither consumes it, so both
// read the argument directly and nothing is copied in between.
//
// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil7isTwice{{[_0-9a-zA-Z]*}}F
// CHECK:           checked_cast_addr_br test_only any P & ~Copyable in {{%.*}} to Big
// CHECK:           checked_cast_addr_br test_only any P & ~Copyable in {{%.*}} to Small
// CHECK-NOT:       copy_addr
// CHECK-NOT:       destroy_addr
// CHECK:         } // end sil function
func isTwice(_ box: borrowing any P & ~Copyable) -> Bool {
  (box is Big) && (box is Small)
}

// MARK: - `case is T` in a switch

// Each case is its own test against the same address; the subject is destroyed
// once at scope exit, not by any of them.
//
// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil8classify{{[_0-9a-zA-Z]*}}F
// CHECK:           checked_cast_addr_br test_only any P & ~Copyable in {{%.*}} to Small
// CHECK:           checked_cast_addr_br test_only any P & ~Copyable in {{%.*}} to Big
// CHECK-NOT:       checked_cast_addr_br take_always
// CHECK-NOT:       checked_cast_addr_br take_on_success
// CHECK-NOT:       checked_cast_addr_br copy_on_success
// CHECK:         } // end sil function
func classify(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is Small: return 1
  case is Big: return 2
  default: return -1
  }
}

// MARK: - `if case is T`

// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil8ifCaseIs{{[_0-9a-zA-Z]*}}F
// CHECK:           checked_cast_addr_br test_only any P & ~Copyable in {{%.*}} to Big
// CHECK-NOT:       copy_addr
// CHECK-NOT:       destroy_addr
// CHECK:         } // end sil function
func ifCaseIs(_ box: borrowing any P & ~Copyable) -> Bool {
  if case is Big = box { return true }
  return false
}

// MARK: - `as?` as a plain value is still consuming; `if let` over it is not

// The value-producing form keeps its take: it hands back a payload, so there is
// no failure edge to leave the subject on.
//
// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil15asOptionalValue{{[_0-9a-zA-Z]*}}F
// CHECK:           checked_cast_addr_br take_always any P & ~Copyable in {{%.*}} to Big in {{%.*}}, bb
// CHECK-NOT:       test_only
// CHECK:         } // end sil function
func asOptionalValue(_ box: consuming any P & ~Copyable) {
  _ = box as? Big
}

// `if let` over `as?` branches on the cast, so it can consume only on success
// and leave the subject alone on the failure edge.
//
// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil10asOptional{{[_0-9a-zA-Z]*}}F
// CHECK:           checked_cast_addr_br take_on_success any P & ~Copyable in {{%.*}} to Big in {{%.*}}, bb
// CHECK-NOT:       test_only
// CHECK:         } // end sil function
func asOptional(_ box: consuming any P & ~Copyable) -> Int {
  if let b = box as? Big { return b.tag }
  return -1
}

// MARK: - A copyable existential gets the same treatment

// canUseTypeTestForIsa() asks only whether the cast uses the address strategy,
// not whether the source is copyable: `is` never needs the payload either way.
// So an opaque *copyable* existential also lowers to a test.
//
// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil15copyableSubject{{[_0-9a-zA-Z]*}}F
// CHECK:           checked_cast_addr_br test_only any CopyableP in {{%.*}} to CopyableS
// CHECK-NOT:       copy_addr
// CHECK:         } // end sil function
protocol CopyableP {}
struct CopyableS: CopyableP {}
func copyableSubject(_ box: any CopyableP) -> Bool {
  box is CopyableS
}

// MARK: - The test carries the target's cast options

// CheckedCastInstOptions are a property of the target type, not of how the cast
// is emitted, and they decide whether the cast *succeeds*: a `Sendable`-
// constrained existential must not be satisfied by an isolated conformance. So
// the test form has to carry them exactly as the value-producing form does --
// dropping them would make `is` and `as?` disagree.
//
// Only reachable now that the test applies to copyable sources: a non-`Copyable`
// source cannot have an existential target, so its options are always default.

protocol SendableRefining: Sendable {}

// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil22genericIsSendableBoundySbxlF
// CHECK:           checked_cast_addr_br [prohibit_isolated_conformances] test_only T in {{%.*}} to any SendableRefining
// CHECK:         } // end sil function
func genericIsSendableBound<T>(_ x: T) -> Bool {
  x is any SendableRefining
}

// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil0B15IsSendableBoundySbAA9CopyableP_pF
// CHECK:           checked_cast_addr_br [prohibit_isolated_conformances] test_only any CopyableP in {{%.*}} to any SendableRefining
// CHECK:         } // end sil function
func existentialIsSendableBound(_ x: any CopyableP) -> Bool {
  x is any SendableRefining
}

// A target with no Sendable constraint keeps the default options, so no marker.
//
// CHECK-LABEL: sil hidden [ossa] @$s36noncopyable_existential_typetest_sil23genericIsUnconstrainedPySbxlF
// CHECK:           checked_cast_addr_br test_only T in {{%.*}} to any CopyableP
// CHECK:         } // end sil function
func genericIsUnconstrainedP<T>(_ x: T) -> Bool {
  x is any CopyableP
}
