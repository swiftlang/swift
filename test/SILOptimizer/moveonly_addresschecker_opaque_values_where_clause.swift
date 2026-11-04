// RUN: %target-swift-frontend -emit-sil -sil-verify-all -verify -target %target-future-triple -enable-experimental-feature NoncopyableCasting %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -verify -target %target-future-triple -enable-experimental-feature NoncopyableCasting -enable-sil-opaque-values %s

// REQUIRES: swift_feature_NoncopyableCasting

// Regression test: under opaque values, a `where` clause in a `switch consume`
// over a non-Copyable existential crashed the move-only address checker.
//
//   Assertion failed: (isInitialized()), function finishedInitializationOfDefs
//
// Address lowering initializes a fresh `alloc_stack` with `copy_addr [init]`
// and marks it only after evaluating the `where` condition, sometimes in a
// later block. The checker recognized such an initialization only in the same
// block and only if no instruction that may write memory came between it and
// the mark. The allocation is used by nothing except the init and the mark, so
// nothing else can write to it.

protocol P: ~Copyable {}
struct Payload: P, ~Copyable { var tag: Int }

func make() -> any P & ~Copyable { Payload(tag: 1) }
func cond() -> Bool { true }

// The condition's call comes between the init and the mark.
func testCall() -> Int {
  let x: any P & ~Copyable = make()
  switch consume x {
  case let y where cond():
    _ = y
    return 1
  case let z:
    _ = z
    return 0
  }
}

// The mark is in a later block than the init.
func testCast() -> Int {
  let x: any P & ~Copyable = make()
  switch consume x {
  case let y as any P & ~Copyable where true: // expected-warning {{'as' test is always true}}
    _ = y
    return 1
  default:
    return 0
  }
}

func testBorrowed(_ x: borrowing any P & ~Copyable) -> Int { // expected-error {{'x' is borrowed and cannot be consumed}}
  switch consume x { // expected-note {{consumed here}}
  case let y as any P & ~Copyable where true: // expected-warning {{'as' test is always true}}
    _ = y
    return 1
  default:
    return 0
  }
}
