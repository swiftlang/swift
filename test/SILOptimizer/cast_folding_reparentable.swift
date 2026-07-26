// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -Xllvm -sil-disable-pass=function-signature-opts -emit-sil -enable-experimental-feature Reparenting %s | %FileCheck %s

// REQUIRES: swift_feature_Reparenting

// A cast to a resilient-parent (@reparentable) protocol must never be folded
// statically: a @reparented extension can introduce the inheritance edge at an
// availability newer than the deployment target, so the conformance is not
// guaranteed to exist at runtime. This holds regardless of how the source
// conforms or what the deployment target is.

@reparentable
public protocol RP {}

public protocol Child: RP {}
extension Child: @reparented RP {}

public final class C: Child {}

// The cast to RP is kept: it is not statically foldable.
// CHECK-LABEL: sil [noinline] @$s25cast_folding_reparentable0A4ToRPySbAA1CCF :
// CHECK: checked_cast_addr_br {{.*}} C in {{.*}} to any RP
// CHECK: } // end sil function '$s25cast_folding_reparentable0A4ToRPySbAA1CCF'
@inline(never)
public func castToRP(_ x: C) -> Bool {
  return x is RP
}

// Casting to an ordinary protocol still folds, even one that inherits from a
// @reparentable protocol: only casts whose target is itself @reparentable are
// pessimized.
// CHECK-LABEL: sil [noinline] @$s25cast_folding_reparentable0A7ToChildySbAA1CCF :
// CHECK-NOT: checked_cast
// CHECK: integer_literal $Builtin.Int1, -1
@inline(never)
public func castToChild(_ x: C) -> Bool {
  return x is Child
}
