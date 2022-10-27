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
// CHECK: [[VALUE:%.*]] = enum $MoveOnlyIntPair, #MoveOnlyIntPair.lhs!enumelt,
// CHECK: [[MV:%.*]] = move_value [lexical] [[VALUE]]
// CHECK: [[MARKED_VALUE:%.*]] = mark_must_check [no_implicit_copy] [[MV]]
// CHECK: debug_value [[MARKED_VALUE]]
// CHECK: } // end sil function '$s21moveonly_enum_literal4testyyF'
func test() {
    let x = MoveOnlyIntPair.lhs(5)
    if value {
        consumeMoveIntPair(x)
    }
}
