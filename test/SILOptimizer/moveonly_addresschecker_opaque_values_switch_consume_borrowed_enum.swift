// RUN: %target-swift-frontend -emit-sil -sil-verify-all -verify %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -verify -enable-sil-opaque-values %s

// Regression test: under opaque values, `switch consume` of a `borrowing`
// enum whose payload is address-only crashed the move-only address checker
// instead of diagnosing the consume.
//
//   Assertion failed: (foundProjection || ... ||
//   isDominatedByPayloadlessSwitchEnumAddrDests(...)),
//   function constructFilteredProjections
//
// Address lowering leaves the `switch_enum_addr` and the payload projections
// on a `begin_access` of the enum, which the projection walk didn't look
// through. And in the `.one` branch, a destroy of the `.two` payload's fields
// is trivially dominated by the switch: that payload doesn't exist there.

protocol P: ~Copyable {}

enum W: ~Copyable {
  case one(any P & ~Copyable)
  case two(Int)
}

func test(_ x: borrowing W) -> Int { // expected-error {{'x' is borrowed and cannot be consumed}}
  switch consume x { // expected-note {{consumed here}}
  case .one(let b):
    _ = b
    return 1
  case .two:
    return 2
  }
}

func testOwned(_ x: consuming W) -> Int {
  switch consume x {
  case .one(let b):
    _ = b
    return 1
  case .two:
    return 2
  }
}
