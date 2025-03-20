// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes \
// RUN:   -parse-stdlib -module-name Swift

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_SuppressedAssociatedTypes

@_marker public protocol Escapable {}

@_silgen_name("imagineInt64")
func imagineInt64() -> Builtin.Int64

precedencegroup AssignmentPrecedence { assignment: true }

protocol P {
  associatedtype E: ~Escapable
  @lifetime(borrow self)
  borrowing func getE() -> E
}

extension P {
  @lifetime(borrow self)
  borrowing func getDefault() -> E {
    return getE()
  }
}

public struct Bits {
  var i = imagineInt64()
}

struct PBits: P {
  func getE() -> Bits { return Bits() }
}

public func pbits_ret_concerete() -> Bits {
  let pbits = PBits()
  return pbits.getDefault()
}

struct NCInt: ~Copyable {
  var value: Builtin.Int64

  init(_ value: Builtin.Int64) { self.value = value }
}

struct NEInt: ~Escapable {
  let value: Builtin.Int64

  @lifetime(copy o)
  init<O: ~Copyable & ~Escapable>(v: Builtin.Int64, o: borrowing O) {
    self.value = v
  }

  // Test a generic storage owner.
  @lifetime(borrow borrowed)
  init(borrowed: borrowing NCInt) {
    self.init(v: borrowed.value, o: borrowed)
  }
}

@lifetime(copy ne)
public func consume_indirect<NE: ~Escapable>(ne: consuming NE) -> NE {
  return ne
}

@lifetime(copy ne)
public func copy_indirect<NE: ~Escapable>(ne: borrowing NE) -> NE {
  return copy ne
}

@lifetime(copy ne)
public func copy_inout<NE: ~Escapable>(ne: inout NE) -> NE {
  return copy ne
}
