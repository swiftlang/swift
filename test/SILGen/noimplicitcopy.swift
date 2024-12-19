// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen -enable-experimental-move-only %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -enable-experimental-move-only %s | %FileCheck -check-prefix=SIL %s

// This test does two different things:
//
// 1. It validates that the emitted SIL From SILGen for noimplicitcopy has the
//    general "form" that we expect.
// 2. Validates that after we reach Canonical SIL, move only has been lowered as
//    appropriately.
//
// NOTE: That today, we assume that non-trivial types and trivial types will
// have moveonlywrapped lowered before Canonical SIL. This will change with time
// as we do the work to turn off the optimizer on non-trivial moveonlywrapped
// things.

//////////////////
// Declarations //
//////////////////

class Klass {
    var i = 8
    func increment() { i += 1 }
}

struct Trivial {
    var value = 5
}

struct NonTrivial {
    var value = Klass()
}


///////////
// Tests //
///////////

///////////////////
// Trivial Tests //
///////////////////

func print2(_ x: Int) {
    print("printInt: \(x + 1)")
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy8printIntyyF : $@convention(thin) () -> () {
// CHECK:   [[X:%.*]] = move_value [var_decl] {{%.*}}
// CHECK:   [[X_MOVEONLY:%.*]] = copyable_to_moveonlywrapper [owned] [[X]] : $Int
// CHECK:   [[X_MOVEONLYWRAPPED_MARKED:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[X_MOVEONLY]]
// CHECK:   [[BORROWED_X_MOVEONLYWRAPPED_MARKED_1:%.*]] = begin_borrow [[X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[BORROWED_X_MOVEONLYWRAPPED_MARKED_2:%.*]] = begin_borrow [[X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[FUNC:%.*]] = function_ref @$sSi1poiyS2i_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Int
// CHECK:   [[GUARANTEED_ESCAPED_X_1:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MOVEONLYWRAPPED_MARKED_1]]
// CHECK:   [[GUARANTEED_ESCAPED_X_2:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MOVEONLYWRAPPED_MARKED_2]]
// CHECK:   apply [[FUNC]]([[GUARANTEED_ESCAPED_X_1]], [[GUARANTEED_ESCAPED_X_2]], {{.*}})
// CHECK:   [[BORROWED_X_MOVEONLYWRAPPED_MARKED:%.*]] = begin_borrow [[X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[GUARANTEED_ESCAPED_X:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   store [[GUARANTEED_ESCAPED_X]] to [trivial] {{%.*}} : $*Int
// CHECK: } // end sil function '$s14noimplicitcopy8printIntyyF'
//
// SIL-LABEL: sil hidden @$s14noimplicitcopy8printIntyyF : $@convention(thin) () -> () {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy8printIntyyF'
func printInt() {
    @_noImplicitCopy let x: Int = 5
    print2(x + x)
    print("printInt: \(x)")
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy11printIntArgyySiF : $@convention(thin) (Int) -> () {
// CHECK: bb0([[ARG:%.*]] : @noImplicitCopy $Int):
// CHECK:   [[ARG_WRAPPED:%.*]] = copyable_to_moveonlywrapper [owned] [[ARG]]
// CHECK:   [[LEXICAL_ARG:%.*]] = move_value [lexical] [[ARG_WRAPPED]]
// CHECK:   [[MARKED_ARG:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[LEXICAL_ARG]]
// CHECK:   [[BORROWED_MARKED_ARG_1:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[BORROWED_MARKED_ARG_2:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[FUNC:%.*]] = function_ref @$sSi1poiyS2i_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Int
// CHECK:   [[GUARANTEED_ESCAPED_X_1:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_MARKED_ARG_1]]
// CHECK:   [[GUARANTEED_ESCAPED_X_2:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_MARKED_ARG_2]]
// CHECK:   apply [[FUNC]]([[GUARANTEED_ESCAPED_X_1]], [[GUARANTEED_ESCAPED_X_2]], {{.*}})
// CHECK: } // end sil function '$s14noimplicitcopy11printIntArgyySiF'
//
// SIL: sil hidden @$s14noimplicitcopy11printIntArgyySiF : $@convention(thin) (Int) -> () {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy11printIntArgyySiF'
func printIntArg(@_noImplicitCopy _ x: Int) {
    print2(x + x)
    print("printInt: \(x)")
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy15callPrintIntArgyyF : $@convention(thin) () -> () {
// CHECK: [[INT_LITERAL_FUNC:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[VALUE:%.*]] = apply [[INT_LITERAL_FUNC]](
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy11printIntArgyySiF : $@convention(thin) (Int) -> ()
// CHECK: apply [[FUNC]]([[VALUE]])
//
// CHECK: [[INT_LITERAL_FUNC:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[VALUE:%.*]] = apply [[INT_LITERAL_FUNC]](
// CHECK: [[MV_VAL:%.*]] = move_value [var_decl] [[VALUE]] : $Int
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy11printIntArgyySiF : $@convention(thin) (Int) -> ()
// CHECK: apply [[FUNC]]([[MV_VAL]])
//
// CHECK: [[Y_BOX:%.*]] = alloc_box ${ var Int }
// CHECK: [[Y_BOX_LIFETIME:%.*]] = begin_borrow [var_decl] [[Y_BOX]]
// CHECK: [[Y_BOX_PROJECT:%.*]] = project_box [[Y_BOX_LIFETIME]]
// CHECK: [[Y_BOX_PROJECT_ACCESS:%.*]] = begin_access [read] [unknown] [[Y_BOX_PROJECT]]
// CHECK: [[Y_VALUE:%.*]] = load [trivial] [[Y_BOX_PROJECT_ACCESS]]
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy11printIntArgyySiF : $@convention(thin) (Int) -> ()
// CHECK: apply [[FUNC]]([[Y_VALUE]])
// CHECK: } // end sil function '$s14noimplicitcopy15callPrintIntArgyyF'
func callPrintIntArg() {
    printIntArg(5)
    let x = 5
    printIntArg(x)
    var y = 6
    y = 6
    printIntArg(y)
    let _ = y
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy16printIntOwnedArgyySinF : $@convention(thin) (Int) -> () {
// CHECK: bb0([[ARG:%.*]] : @noImplicitCopy $Int):
// CHECK:   [[ARG_WRAPPED:%.*]] = copyable_to_moveonlywrapper [owned] [[ARG]]
// CHECK:   [[LEXICAL_ARG:%.*]] = move_value [lexical] [[ARG_WRAPPED]]
// CHECK:   [[MARKED_ARG:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[LEXICAL_ARG]]
// CHECK:   [[BORROWED_MARKED_ARG_1:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[BORROWED_MARKED_ARG_2:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[FUNC:%.*]] = function_ref @$sSi1poiyS2i_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Int
// CHECK:   [[GUARANTEED_ESCAPED_X_1:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_MARKED_ARG_1]]
// CHECK:   [[GUARANTEED_ESCAPED_X_2:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_MARKED_ARG_2]]
// CHECK:   apply [[FUNC]]([[GUARANTEED_ESCAPED_X_1]], [[GUARANTEED_ESCAPED_X_2]], {{.*}})
// CHECK: } // end sil function '$s14noimplicitcopy16printIntOwnedArgyySinF'
//
// SIL-LABEL: sil hidden @$s14noimplicitcopy16printIntOwnedArgyySinF : $@convention(thin) (Int) -> () {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy16printIntOwnedArgyySinF'
func printIntOwnedArg(@_noImplicitCopy _ x: __owned Int) {
    print2(x + x)
    print("printInt: \(x)")
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy20callPrintIntOwnedArgyyF : $@convention(thin) () -> () {
// CHECK: [[INT_LITERAL_FUNC:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[VALUE:%.*]] = apply [[INT_LITERAL_FUNC]](
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy16printIntOwnedArgyySinF : $@convention(thin) (Int) -> ()
// CHECK: apply [[FUNC]]([[VALUE]])
//
// CHECK: [[INT_LITERAL_FUNC:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[VALUE:%.*]] = apply [[INT_LITERAL_FUNC]](
// CHECK: [[MV_VAL:%.*]] = move_value [var_decl] [[VALUE]] : $Int
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy16printIntOwnedArgyySinF : $@convention(thin) (Int) -> ()
// CHECK: apply [[FUNC]]([[MV_VAL]])
//
// CHECK: [[Y_BOX:%.*]] = alloc_box ${ var Int }
// CHECK: [[Y_BOX_LIFETIME:%.*]] = begin_borrow [var_decl] [[Y_BOX]]
// CHECK: [[Y_BOX_PROJECT:%.*]] = project_box [[Y_BOX_LIFETIME]]
// CHECK: [[Y_BOX_PROJECT_ACCESS:%.*]] = begin_access [read] [unknown] [[Y_BOX_PROJECT]]
// CHECK: [[Y_VALUE:%.*]] = load [trivial] [[Y_BOX_PROJECT_ACCESS]]
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy16printIntOwnedArgyySinF : $@convention(thin) (Int) -> ()
// CHECK: apply [[FUNC]]([[Y_VALUE]])
// } // end sil function '$s14noimplicitcopy15callPrintIntArgyy'
func callPrintIntOwnedArg() {
    printIntOwnedArg(5)
    let x = 5
    printIntOwnedArg(x)
    var y = 5
    y = 5
    printIntOwnedArg(y)
    let _ = y
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy17printIntArgThrowsyySiKF : $@convention(thin) (Int) -> @error any Error {
// CHECK: bb0([[ARG:%.*]] : @noImplicitCopy $Int):
// CHECK:   [[ARG_WRAPPED:%.*]] = copyable_to_moveonlywrapper [owned] [[ARG]]
// CHECK:   [[LEXICAL_ARG:%.*]] = move_value [lexical] [[ARG_WRAPPED]]
// CHECK:   [[MARKED_ARG:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[LEXICAL_ARG]]
// CHECK:   [[BORROWED_MARKED_ARG_1:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[BORROWED_MARKED_ARG_2:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[FUNC:%.*]] = function_ref @$sSi1poiyS2i_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Int
// CHECK:   [[GUARANTEED_ESCAPED_X_1:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_MARKED_ARG_1]]
// CHECK:   [[GUARANTEED_ESCAPED_X_2:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_MARKED_ARG_2]]
// CHECK:   apply [[FUNC]]([[GUARANTEED_ESCAPED_X_1]], [[GUARANTEED_ESCAPED_X_2]], {{.*}})
// CHECK: } // end sil function '$s14noimplicitcopy17printIntArgThrowsyySiKF'
//
// SIL-LABEL: sil hidden @$s14noimplicitcopy17printIntArgThrowsyySiKF : $@convention(thin) (Int) -> @error any Error {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy17printIntArgThrowsyySiKF'
func printIntArgThrows(@_noImplicitCopy _ x: Int) throws {
    print2(x + x)
    print("printIntArgThrows: \(x)")
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy21callPrintIntArgThrowsyyKF : $@convention(thin) () -> @error any Error {
// CHECK: [[INT_LITERAL_FUNC:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[VALUE:%.*]] = apply [[INT_LITERAL_FUNC]](
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy17printIntArgThrowsyySiKF : $@convention(thin) (Int) -> @error any Error
// CHECK: try_apply [[FUNC]]([[VALUE]])
//
// CHECK: [[INT_LITERAL_FUNC:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[VALUE:%.*]] = apply [[INT_LITERAL_FUNC]](
// CHECK: [[MV_VAL:%.*]] = move_value [var_decl] [[VALUE]] : $Int
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy17printIntArgThrowsyySiKF : $@convention(thin) (Int) -> @error any Error
// CHECK: try_apply [[FUNC]]([[MV_VAL]])
//
// CHECK: [[Y_BOX:%.*]] = alloc_box ${ var Int }
// CHECK: [[Y_BOX_LIFETIME:%.*]] = begin_borrow [var_decl] [[Y_BOX]]
// CHECK: [[Y_BOX_PROJECT:%.*]] = project_box [[Y_BOX_LIFETIME]]
// CHECK: [[Y_BOX_PROJECT_ACCESS:%.*]] = begin_access [read] [unknown] [[Y_BOX_PROJECT]]
// CHECK: [[Y_VALUE:%.*]] = load [trivial] [[Y_BOX_PROJECT_ACCESS]]
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy17printIntArgThrowsyySiKF : $@convention(thin) (Int) -> @error any Error
// CHECK: try_apply [[FUNC]]([[Y_VALUE]])
// CHECK: } // end sil function '$s14noimplicitcopy21callPrintIntArgThrowsyyKF'
func callPrintIntArgThrows() throws {
    try printIntArgThrows(5)
    let x = 5
    try printIntArgThrows(x)
    var y = 5
    y = 5
    try printIntArgThrows(y)
    let _ = y
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy22printIntOwnedArgThrowsyySinKF : $@convention(thin) (Int) -> @error any Error {
// CHECK: bb0(%0 : @noImplicitCopy $Int):
// CHECK: } // end sil function '$s14noimplicitcopy22printIntOwnedArgThrowsyySinKF'
//
// SIL-LABEL: sil hidden @$s14noimplicitcopy22printIntOwnedArgThrowsyySinKF : $@convention(thin) (Int) -> @error any Error {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy22printIntOwnedArgThrowsyySinKF'
func printIntOwnedArgThrows(@_noImplicitCopy _ x: __owned Int) throws {
    print2(x + x)
    print("printIntOwnedArgThrows: \(x)")
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy26callPrintIntOwnedArgThrowsyyKF : $@convention(thin) () -> @error any Error {
// CHECK: [[INT_LITERAL_FUNC:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[VALUE:%.*]] = apply [[INT_LITERAL_FUNC]](
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy22printIntOwnedArgThrowsyySinKF : $@convention(thin) (Int) -> @error any Error
// CHECK: try_apply [[FUNC]]([[VALUE]])
//
// CHECK: [[INT_LITERAL_FUNC:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[VALUE:%.*]] = apply [[INT_LITERAL_FUNC]](
// CHECK: [[MV_VAL:%.*]] = move_value [var_decl] [[VALUE]] : $Int
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy22printIntOwnedArgThrowsyySinKF : $@convention(thin) (Int) -> @error any Error
// CHECK: try_apply [[FUNC]]([[MV_VAL]])
//
// CHECK: [[Y_BOX:%.*]] = alloc_box ${ var Int }
// CHECK: [[Y_BOX_LIFETIME:%.*]] = begin_borrow [var_decl] [[Y_BOX]]
// CHECK: [[Y_BOX_PROJECT:%.*]] = project_box [[Y_BOX_LIFETIME]]
// CHECK: [[Y_BOX_PROJECT_ACCESS:%.*]] = begin_access [read] [unknown] [[Y_BOX_PROJECT]]
// CHECK: [[Y_VALUE:%.*]] = load [trivial] [[Y_BOX_PROJECT_ACCESS]]
// CHECK: [[FUNC:%.*]] = function_ref @$s14noimplicitcopy22printIntOwnedArgThrowsyySinKF : $@convention(thin) (Int) -> @error any Error
// CHECK: try_apply [[FUNC]]([[Y_VALUE]])
// CHECK: } // end sil function '$s14noimplicitcopy26callPrintIntOwnedArgThrowsyyKF'
func callPrintIntOwnedArgThrows() throws {
    try printIntOwnedArgThrows(5)
    let x = 5
    try printIntOwnedArgThrows(x)
    var y = 5
    y = 5
    try printIntOwnedArgThrows(y)
    let _ = y
}

func useClosureInt(_ f: (Int) -> ()) {}

// Make sure that our closure has the proper convention.
//
// CHECK-LABEL: sil private [ossa] @$s14noimplicitcopy14callClosureIntyyFySicfU_ : $@convention(thin) (Int) -> () {
// CHECK: bb0([[X:%.*]] : @noImplicitCopy $Int):
// CHECK:   [[WRAPPED_X:%.*]] = copyable_to_moveonlywrapper [owned] [[X]]
// CHECK:   [[MOVED_X:%.*]] = move_value [lexical] [[WRAPPED_X]]
// CHECK:   [[MARKED_X:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[MOVED_X]]
// CHECK:   [[BORROWED_X_MARKED_1:%.*]] = begin_borrow [[MARKED_X]]
// CHECK:   [[BORROWED_X_MARKED_2:%.*]] = begin_borrow [[MARKED_X]]
// CHECK:   [[FUNC:%.*]] = function_ref @$sSi1poiyS2i_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Int
// CHECK:   [[GUARANTEED_ESCAPED_X_1:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MARKED_1]]
// CHECK:   [[GUARANTEED_ESCAPED_X_2:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MARKED_2]]
// CHECK:   apply [[FUNC]]([[GUARANTEED_ESCAPED_X_1]], [[GUARANTEED_ESCAPED_X_2]], {{.*}})
// CHECK:   [[BORROWED_X_MARKED:%.*]] = begin_borrow [[MARKED_X]]
// CHECK:   [[GUARANTEED_ESCAPED_X:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MARKED]]
// CHECK:   store [[GUARANTEED_ESCAPED_X]] to [trivial] {{%.*}} : $*Int
// CHECK: } // end sil function '$s14noimplicitcopy14callClosureIntyyFySicfU_'
//
// SIL-LABEL: sil private @$s14noimplicitcopy14callClosureIntyyFySicfU_ : $@convention(thin) (Int) -> () {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy14callClosureIntyyFySicfU_'
func callClosureInt() {
    let f = { (@_noImplicitCopy _ x: Int) -> () in
        print2(x + x)
        print("closure: \(x)")
    }
    f(5)
    let x = 5
    f(x)
    var y = 5
    y = 5
    f(y)
    let _ = y
    useClosureInt(f)
}

// CHECK-LABEL: sil private [ossa] @$s14noimplicitcopy19callClosureIntOwnedyyFySincfU_ : $@convention(thin) (Int) -> () {
// CHECK: bb0([[ARG:%.*]] : @noImplicitCopy $Int):
// CHECK:   [[WRAPPED_ARG:%.*]] = copyable_to_moveonlywrapper [owned] [[ARG]]
// CHECK:   [[MOVED_ARG:%.*]] = move_value [lexical] [[WRAPPED_ARG]]
// CHECK:   [[MARKED_ARG:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[MOVED_ARG]]
// CHECK:   [[BORROWED_ARG_MARKED_1:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[BORROWED_ARG_MARKED_2:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[FUNC:%.*]] = function_ref @$sSi1poiyS2i_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Int
// CHECK:   [[GUARANTEED_ESCAPED_ARG_1:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_ARG_MARKED_1]]
// CHECK:   [[GUARANTEED_ESCAPED_ARG_2:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_ARG_MARKED_2]]
// CHECK:   apply [[FUNC]]([[GUARANTEED_ESCAPED_ARG_1]], [[GUARANTEED_ESCAPED_ARG_2]], {{.*}})
// CHECK:   [[BORROWED_ARG_MARKED:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[GUARANTEED_ESCAPED_ARG:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_ARG_MARKED]]
// CHECK:   store [[GUARANTEED_ESCAPED_ARG]] to [trivial] {{%.*}} : $*Int
// CHECK: } // end sil function '$s14noimplicitcopy19callClosureIntOwnedyyFySincfU_'
//
// SIL-LABEL: sil private @$s14noimplicitcopy19callClosureIntOwnedyyFySincfU_ : $@convention(thin) (Int) -> () {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy19callClosureIntOwnedyyFySincfU_'
func callClosureIntOwned() {
    let f = { (@_noImplicitCopy _ x: __owned Int) -> () in
        print2(x + x)
        print("closure: \(x)")
    }
    f(5)
    let x = 5
    f(x)
    var y = 5
    y = 5
    f(y)
    let _ = y
    useClosureInt(f)
}

/////////////////
// Non Trivial //
/////////////////

// NOTE: MOW expands to MOVEONLYWRAPPED
//
// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy10printKlassyyF : $@convention(thin) () -> () {
// CHECK:   [[X:%.*]] = move_value [lexical] [var_decl] {{%[0-9]+}} : $Klass
// CHECK:   [[X_MOVEONLYWRAPPED:%.*]] = copyable_to_moveonlywrapper [owned] [[X]]
// CHECK:   [[X_MOVEONLYWRAPPED_MARKED:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[X_MOVEONLYWRAPPED]]
// CHECK:   [[BORROWED_X_MOVEONLYWRAPPED_MARKED:%.*]] = begin_borrow [[X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[FUNC:%.*]] = class_method [[BORROWED_X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[GUARANTEED_ESCAPED_X:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   apply [[FUNC]]([[GUARANTEED_ESCAPED_X]])
//
// CHECK:   [[BORROWED_X_MOVEONLYWRAPPED_MARKED:%.*]] = begin_borrow [[X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[FUNC:%.*]] = class_method [[BORROWED_X_MOVEONLYWRAPPED_MARKED]] :
// CHECK:   [[GUARANTEED_ESCAPED_X:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   apply [[FUNC]]([[GUARANTEED_ESCAPED_X]])
//
// CHECK:   [[BORROWED_X_MOVEONLYWRAPPED_MARKED:%.*]] = begin_borrow [[X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[GUARANTEED_ESCAPED_X:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   store_borrow [[GUARANTEED_ESCAPED_X]] to
// CHECK: } // end sil function '$s14noimplicitcopy10printKlassyyF'
//
// SIL-LABEL: sil hidden @$s14noimplicitcopy10printKlassyyF : $@convention(thin) () -> () {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy10printKlassyyF'
func printKlass() {
    @_noImplicitCopy let x = Klass()
    x.increment()
    x.increment()
    print("printKlass: \(x)")
    print("printKlass: \(x.i)")
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy13printKlassArgyyAA0C0CF : $@convention(thin) (@guaranteed Klass) -> () {
// CHECK: bb0([[ARG:%.*]] : @noImplicitCopy @guaranteed $Klass):
// CHECK:   [[WRAPPED_ARG:%.*]] = copyable_to_moveonlywrapper [guaranteed] [[ARG]]
// CHECK:   [[COPIED_WRAPPED_ARG:%.*]] = copy_value [[WRAPPED_ARG]]
// CHECK:   [[MARKED_ARG:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPIED_WRAPPED_ARG]]
// CHECK: } // end sil function '$s14noimplicitcopy13printKlassArgyyAA0C0CF'
//
// SIL-LABEL: sil hidden @$s14noimplicitcopy13printKlassArgyyAA0C0CF : $@convention(thin) (@guaranteed Klass) -> () {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy13printKlassArgyyAA0C0CF'
func printKlassArg(@_noImplicitCopy _ x: Klass) {
    x.increment()
    x.increment()
    print("printKlass: \(x)")
    print("printKlass: \(x.i)")
}

func callPrintKlassArg() {
    printKlassArg(Klass())
    let x = Klass()
    printKlassArg(x)
    var y = Klass()
    y = Klass()
    printKlassArg(y)
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy18printKlassOwnedArgyyAA0C0CnF : $@convention(thin) (@owned Klass) -> () {
// CHECK: bb0([[ARG:%.*]] : @noImplicitCopy @owned $Klass):
// CHECK:   [[WRAPPED_ARG:%.*]] = copyable_to_moveonlywrapper [owned] [[ARG]]
// CHECK:   [[MOVED_ARG:%.*]] = move_value [lexical] [[WRAPPED_ARG]]
// CHECK:   [[MARKED_ARG:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[MOVED_ARG]]
// CHECK: } // end sil function '$s14noimplicitcopy18printKlassOwnedArgyyAA0C0CnF'
//
// SIL-LABEL: sil hidden @$s14noimplicitcopy18printKlassOwnedArgyyAA0C0CnF : $@convention(thin) (@owned Klass) -> () {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy18printKlassOwnedArgyyAA0C0CnF'
func printKlassOwnedArg(@_noImplicitCopy _ x: __owned Klass) {
    x.increment()
    x.increment()
    print("printKlass: \(x)")
    print("printKlass: \(x.i)")
}

func callPrintKlassOwnedArg() {
    printKlassOwnedArg(Klass())
    let x = Klass()
    printKlassOwnedArg(x)
    var y = Klass()
    y = Klass()
    printKlassOwnedArg(y)
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy19printKlassArgThrowsyyAA0C0CKF : $@convention(thin) (@guaranteed Klass) -> @error any Error {
// CHECK: bb0([[ARG:%.*]] : @noImplicitCopy @guaranteed $Klass):
// CHECK:   [[WRAPPED_ARG:%.*]] = copyable_to_moveonlywrapper [guaranteed] [[ARG]]
// CHECK:   [[COPIED_ARG:%.*]] = copy_value [[WRAPPED_ARG]]
// CHECK:   [[MARKED_ARG:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPIED_ARG]]
// CHECK:   [[BORROWED_MARKED_ARG:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[UNWRAPPED_ARG:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_MARKED_ARG]]
// CHECK:   apply {{%.*}}([[UNWRAPPED_ARG]]) :
// CHECK: } // end sil function '$s14noimplicitcopy19printKlassArgThrowsyyAA0C0CKF'
//
// SIL-LABEL: sil hidden @$s14noimplicitcopy19printKlassArgThrowsyyAA0C0CKF : $@convention(thin) (@guaranteed Klass) -> @error any Error {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy19printKlassArgThrowsyyAA0C0CKF'
func printKlassArgThrows(@_noImplicitCopy _ x: Klass) throws {
    x.increment()
    x.increment()
    print("printKlassArgThrows: \(x)")
}

func callPrintKlassArgThrows() throws {
    try printKlassArgThrows(Klass())
    let x = Klass()
    try printKlassArgThrows(x)
    var y = Klass()
    y = Klass()
    try printKlassArgThrows(y)
}

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy24printKlassOwnedArgThrowsyyAA0C0CnKF : $@convention(thin) (@owned Klass) -> @error any Error {
// CHECK: bb0([[ARG:%.*]] : @noImplicitCopy @owned $Klass):
// CHECK:   [[WRAPPED_ARG:%.*]] = copyable_to_moveonlywrapper [owned] [[ARG]]
// CHECK:   [[MOVED_ARG:%.*]] = move_value [lexical] [[WRAPPED_ARG]]
// CHECK:   [[MARKED_ARG:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[MOVED_ARG]]
// CHECK:   [[BORROWED_MARKED_ARG:%.*]] = begin_borrow [[MARKED_ARG]]
// CHECK:   [[UNWRAPPED_ARG:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_MARKED_ARG]]
// CHECK:   apply {{%.*}}([[UNWRAPPED_ARG]]) :
// CHECK: } // end sil function '$s14noimplicitcopy24printKlassOwnedArgThrowsyyAA0C0CnKF'
//
// SIL-LABEL: sil hidden @$s14noimplicitcopy24printKlassOwnedArgThrowsyyAA0C0CnKF : $@convention(thin) (@owned Klass) -> @error any Error {
// SIL-NOT: @moveOnly
// SIL: } // end sil function '$s14noimplicitcopy24printKlassOwnedArgThrowsyyAA0C0CnKF'
func printKlassOwnedArgThrows(@_noImplicitCopy _ x: __owned Klass) throws {
    x.increment()
    x.increment()
    print("printKlassOwnedArgThrows: \(x)")
}

func callPrintKlassOwnedOwnedArgThrows() throws {
    try printKlassOwnedArgThrows(Klass())
    let x = Klass()
    try printKlassOwnedArgThrows(x)
    var y = Klass()
    y = Klass()
    try printKlassOwnedArgThrows(y)
}

//////////////
// Gep Test //
//////////////

// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy19test_nontrivial_gepyyAA7TrivialVF : $@convention(thin) (Trivial) -> () {
// CHECK: bb0([[ARG:%.*]] : $Trivial):
// CHECK:   [[MV:%.*]] = move_value [var_decl] [[ARG]] : $Trivial
// CHECK:   [[WRAPPED:%.*]] = copyable_to_moveonlywrapper [owned] [[MV]]
// CHECK:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[WRAPPED]]
// CHECK:   [[BORROW:%.*]] = begin_borrow [[MARKED]]
// CHECK:   [[EXT:%.*]] = struct_extract [[BORROW]]
// CHECK:   [[UNWRAPPED:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[EXT]]
// CHECK:   apply {{%.*}}([[UNWRAPPED]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   destroy_value [[MARKED]]
// CHECK: } // end sil function '$s14noimplicitcopy19test_nontrivial_gepyyAA7TrivialVF'
func test_nontrivial_gep(_ x: Trivial) {
    @_noImplicitCopy let y = x
    print2(y.value)
}
