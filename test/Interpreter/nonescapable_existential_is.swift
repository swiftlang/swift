// `any P & ~Copyable & ~Escapable` is spelled with inverses, so its metadata
// kind is `ExtendedExistential` -- the same path whose mishandling made every
// `is` answer `false` before `unwrapExtendedExistentialForTest` existed. The
// noncopyable tests cover one inverse; this covers two, and `~Escapable` alone.
//
// Nonescapable payloads cannot be copied out, so `is` is the only cast form that
// applies to them at all, which makes the non-consuming lowering the only way to
// ask this question.

// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting -enable-experimental-feature NonescapableTypes -enable-experimental-feature LifetimeDependence -enable-experimental-feature Lifetimes) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -target %target-future-triple -enable-experimental-feature NoncopyableCasting -enable-experimental-feature NonescapableTypes -enable-experimental-feature LifetimeDependence -enable-experimental-feature Lifetimes) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: swift_feature_NonescapableTypes
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_Lifetimes

// These require `swift_dynamicCastTest`, which only a new enough runtime has.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// The -O line above becomes -O *plus* opaque values in this mode, and that
// combination trips a SILCombine debug-info bug: after a move-only box is
// promoted away, the address-based debug_value describing it is left on an
// undef alongside the object-based one recording the folded value, and the two
// conflict. Unrelated to casting -- the cast folds correctly -- and reproduced
// standalone in swift-test-baselines/bugs/ov-O-duplicate-debug-value.swift.
// The -Onone line passes under opaque values; it is only the pairing with -O.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

protocol P: ~Copyable, ~Escapable {}

/// Neither copyable nor escapable.
struct NCNE: ~Copyable, ~Escapable, P {
  var t: Int
  @lifetime(immortal) init(t: Int) { self.t = t }
}

/// Nonescapable but copyable.
struct NE: ~Escapable, P {
  var t: Int
  @lifetime(immortal) init(t: Int) { self.t = t }
}

/// Noncopyable but escapable.
struct NCo: ~Copyable, P {
  var t: Int
}

/// Ordinary.
struct Plain: P { var t: Int }

// MARK: - both inverses on the existential

func isNCNE(_ box: borrowing any P & ~Copyable & ~Escapable) -> Bool { box is NCNE }
func isNE(_ box: borrowing any P & ~Copyable & ~Escapable) -> Bool { box is NE }
func isNCo(_ box: borrowing any P & ~Copyable & ~Escapable) -> Bool { box is NCo }
func isPlain(_ box: borrowing any P & ~Copyable & ~Escapable) -> Bool { box is Plain }

func classify(_ box: borrowing any P & ~Copyable & ~Escapable) -> Int {
  if box is NCNE { return 1 }
  if box is NE { return 2 }
  if box is NCo { return 3 }
  if box is Plain { return 4 }
  return 0
}

do {
  let b: any P & ~Copyable & ~Escapable = NCNE(t: 1)
  // CHECK: NCNE: true false false false 1
  print("NCNE:", isNCNE(b), isNE(b), isNCo(b), isPlain(b), classify(b))
}
do {
  let b: any P & ~Copyable & ~Escapable = NE(t: 2)
  // CHECK-NEXT: NE: false true false false 2
  print("NE:", isNCNE(b), isNE(b), isNCo(b), isPlain(b), classify(b))
}
do {
  let b: any P & ~Copyable & ~Escapable = NCo(t: 3)
  // CHECK-NEXT: NCo: false false true false 3
  print("NCo:", isNCNE(b), isNE(b), isNCo(b), isPlain(b), classify(b))
}
do {
  let b: any P & ~Copyable & ~Escapable = Plain(t: 4)
  // CHECK-NEXT: Plain: false false false true 4
  print("Plain:", isNCNE(b), isNE(b), isNCo(b), isPlain(b), classify(b))
}

// MARK: - the subject survives the test

// Two statements rather than `(box is Plain) && (box is Plain)`: `&&` takes its
// right operand as an autoclosure, and capturing a borrowed noncopyable
// existential copies it under -enable-sil-opaque-values -- a SILGen capture bug
// unrelated to casting. Testing twice is the point here.
func twice(_ box: borrowing any P & ~Copyable & ~Escapable) -> Bool {
  let first = box is Plain
  let second = box is Plain
  return first && second
}
do {
  let b: any P & ~Copyable & ~Escapable = Plain(t: 5)
  // CHECK-NEXT: twice: true
  print("twice:", twice(b))
}

// MARK: - `~Escapable` alone, without `~Copyable`

func isNEOnly(_ box: borrowing any P & ~Escapable) -> Bool { box is NE }
func isPlainFromNEOnly(_ box: borrowing any P & ~Escapable) -> Bool { box is Plain }

do {
  let b: any P & ~Escapable = NE(t: 6)
  // CHECK-NEXT: NEOnly: true false
  print("NEOnly:", isNEOnly(b), isPlainFromNEOnly(b))
}
do {
  let b: any P & ~Escapable = Plain(t: 7)
  // CHECK-NEXT: PlainFromNEOnly: false true
  print("PlainFromNEOnly:", isNEOnly(b), isPlainFromNEOnly(b))
}

// MARK: - pattern form

func viaSwitch(_ box: borrowing any P & ~Copyable & ~Escapable) -> Int {
  switch box {
  case is NCNE: return 1
  case is NE: return 2
  case is Plain: return 4
  default: return 0
  }
}
do {
  let a: any P & ~Copyable & ~Escapable = NCNE(t: 8)
  let b: any P & ~Copyable & ~Escapable = Plain(t: 9)
  // CHECK-NEXT: switch: 1 4
  print("switch:", viaSwitch(a), viaSwitch(b))
}
