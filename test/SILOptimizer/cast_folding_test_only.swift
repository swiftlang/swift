// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -emit-sil %s -target %target-future-triple -enable-experimental-feature NoncopyableCasting | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting

// A `checked_cast_addr_br test_only` whose outcome is statically known should
// fold to a constant, exactly as the value-producing cast kinds do -- see
// cast_folding.swift, whose whole purpose is that `is` checks collapse at
// compile time.
//
// Only -O is checked: the fold needs an earlier pass to expose the concrete type
// behind the existential, so at -Onone dynamicCastResult cannot decide and the
// cast legitimately survives.

public protocol P: ~Copyable {}

public struct NC: ~Copyable, P {
  var t: Int
  public init(t: Int) { self.t = t }
}
public struct Other: ~Copyable, P { public init() {} }

// The existential is initialized right here, so the dynamic type is known.
//
// CHECK-LABEL: sil [noinline] @$s22cast_folding_test_only12provablyTrueSbyF
// CHECK:         %0 = integer_literal $Builtin.Int1, -1
// CHECK-NOT:     checked_cast_addr_br
// CHECK:       } // end sil function
@inline(never) public func provablyTrue() -> Bool {
  let box: any P & ~Copyable = NC(t: 1)
  return box is NC
}

// CHECK-LABEL: sil [noinline] @$s22cast_folding_test_only13provablyFalseSbyF
// CHECK:         %0 = integer_literal $Builtin.Int1, 0
// CHECK-NOT:     checked_cast_addr_br
// CHECK:       } // end sil function
@inline(never) public func provablyFalse() -> Bool {
  let box: any P & ~Copyable = NC(t: 1)
  return box is Other
}

// Control: with the dynamic type unknown, the test must survive.
//
// CHECK-LABEL: sil [noinline] @$s22cast_folding_test_only11notProvableySbAA1P_pRi_s_XPF
// CHECK:         checked_cast_addr_br test_only
// CHECK:       } // end sil function
@inline(never) public func notProvable(_ box: borrowing any P & ~Copyable) -> Bool {
  return box is NC
}
