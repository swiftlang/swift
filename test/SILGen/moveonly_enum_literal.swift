// RUN: %target-swift-frontend -enable-experimental-move-only -emit-silgen %s | %FileCheck %s

// This test makes sure that we properly setup enums when we construct moveonly
// enums from literals.

@_moveOnly
enum MoveOnlyIntPair {
case lhs(Int)
case rhs(Int)
}

func consumeMoveIntPair(_ x: __owned MoveOnlyIntPair) {}

var value: Bool { false }

// CHECK-LABEL: sil hidden [ossa] @$s21moveonly_enum_literal4testyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[PROJECT:%.*]] = project_box [[BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT]]
// CHECK: [[VALUE:%.*]] = enum $MoveOnlyIntPair, #MoveOnlyIntPair.lhs!enumelt,
// CHECK: store [[VALUE]] to [init] [[MARKED_ADDR]]
// CHECK: } // end sil function '$s21moveonly_enum_literal4testyyF'
func test() {
    let x = MoveOnlyIntPair.lhs(5)
    if value {
        consumeMoveIntPair(x)
    }
}
REQUIRES: updating_for_owned_noescape
