// A plain (non-extended) existential requires its payload to be `Copyable` and
// `Escapable` -- there is nowhere in an `ExistentialTypeMetadata` container to
// record a suppression. `classifyDynamicCastToProtocol` decided feasibility from
// the conformance alone, so a cast of a `~Escapable` value to `any P` folded to a
// static `true` purely because the conformance exists. The runtime rejects that
// cast, which made the fold a miscompile rather than merely imprecise.
//
// Both optimization levels are checked, because the fold happens in
// classifyDynamicCast and so applies at -Onone too.

// RUN: %target-swift-frontend -Onone -Xllvm -sil-print-types -emit-sil %s -enable-experimental-feature Lifetimes | %FileCheck %s
// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -emit-sil %s -enable-experimental-feature Lifetimes | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

public protocol P: ~Copyable, ~Escapable {}

/// Nonescapable, and conforming to `P` -- so the conformance check succeeds while
/// the container still cannot hold it.
public struct NE: P, ~Escapable {
  var t: Int
  @_lifetime(immortal) public init(_ t: Int) { self.t = t }
}

/// Copyable and escapable, so every cast below must keep its old answer.
public struct OK: P { public init() {} }

// MARK: - a nonescapable source cannot inhabit a plain existential

// CHECK-LABEL: sil{{.*}}@$s{{.*}}8neToAnyPySbAA2NEVnF
// CHECK: [[R:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK: struct $Bool ([[R]] : $Builtin.Int1)
// CHECK: } // end sil function '$s{{.*}}8neToAnyPySbAA2NEVnF'
public func neToAnyP(_ x: consuming NE) -> Bool { x is any P }

// CHECK-LABEL: sil{{.*}}@$s{{.*}}7neToAnyySbAA2NEVnF
// CHECK: [[R:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK: } // end sil function '$s{{.*}}7neToAnyySbAA2NEVnF'
public func neToAny(_ x: consuming NE) -> Bool { x is Any }

// CHECK-LABEL: sil{{.*}}@$s{{.*}}13neToAnyObjectySbAA2NEVnF
// CHECK: [[R:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK: } // end sil function '$s{{.*}}13neToAnyObjectySbAA2NEVnF'
public func neToAnyObject(_ x: consuming NE) -> Bool { x is AnyObject }

// MARK: - but it does inhabit one that suppresses the requirement

// An extended existential carries the inverse in its requirement signature, so
// this must still fold to `true` rather than being caught by the new rule.
// CHECK-LABEL: sil{{.*}}@$s{{.*}}16neToSuppressingPySbAA2NEVnF
// CHECK: [[R:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: } // end sil function '$s{{.*}}16neToSuppressingPySbAA2NEVnF'
public func neToSuppressingP(_ x: consuming NE) -> Bool { x is any P & ~Escapable }

// MARK: - a copyable, escapable source is unaffected

// CHECK-LABEL: sil{{.*}}@$s{{.*}}8okToAnyPySbAA2OKVnF
// CHECK: [[R:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: } // end sil function '$s{{.*}}8okToAnyPySbAA2OKVnF'
public func okToAnyP(_ x: consuming OK) -> Bool { x is any P }

// CHECK-LABEL: sil{{.*}}@$s{{.*}}7okToAnyySbAA2OKVnF
// CHECK: [[R:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: } // end sil function '$s{{.*}}7okToAnyySbAA2OKVnF'
public func okToAny(_ x: consuming OK) -> Bool { x is Any }

// MARK: - an existential source must not be folded

// The static type says nothing about the value inside: `any P & ~Escapable` can
// hold an `OK`, which does inhabit `Any`. So this has to reach the runtime rather
// than fold either way.
// CHECK-LABEL: sil{{.*}}@$s{{.*}}10boxSubjectySbAA1P_pRi0_s_XPnF
// CHECK-NOT: integer_literal $Builtin.Int1
// CHECK: checked_cast_addr_br
// CHECK: } // end sil function '$s{{.*}}10boxSubjectySbAA1P_pRi0_s_XPnF'
public func boxSubject(_ x: consuming any P & ~Escapable) -> Bool { x is Any }

// An archetype is equally undecidable: `S` may be bound to an escapable type.
// CHECK-LABEL: sil{{.*}}@$s{{.*}}16archetypeSubjectySbxnRi0_zlF
// CHECK-NOT: integer_literal $Builtin.Int1
// CHECK: checked_cast_addr_br
public func archetypeSubject<S: ~Escapable>(_ x: consuming S) -> Bool { x is Any }
