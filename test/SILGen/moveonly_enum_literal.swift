// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all %s

// This test makes sure that we properly setup enums when we construct moveonly
// enums from literals.

enum MoveOnlyIntPair: ~Copyable {
case lhs(Int)
case rhs(Int)
}

func consumeMoveIntPair(_ x: __owned MoveOnlyIntPair) {}

var value: Bool { false }

// CHECK-LABEL: sil hidden [ossa] @$s21moveonly_enum_literal4testyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
// CHECK: [[VALUE:%.*]] = enum $MoveOnlyIntPair, #MoveOnlyIntPair.lhs!enumelt,
// CHECK: store [[VALUE]] to [init] [[PROJECT]]
//
// CHECK: [[MARKED_VALUE:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: } // end sil function '$s21moveonly_enum_literal4testyyF'
func test() {
    let x = MoveOnlyIntPair.lhs(5)
    if value {
        consumeMoveIntPair(x)
    }
}
