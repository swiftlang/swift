// RUN: %target-swift-frontend -emit-silgen -enable-experimental-move-only %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////

class Klass {
    var i = 8
    func increment() { i += 1 }
}

///////////
// Tests //
///////////
func print2(_ x: Int) {
    print("printInt: \(x + 1)")
}

// CHECK: sil hidden [ossa] @$s14noimplicitcopy8printIntyyF : $@convention(thin) () -> () {
// CHECK:   [[X_MOVEONLY:%.*]] = copyable_to_moveonlywrapper {{%.*}} : $Int
// CHECK:   [[X:%.*]] = begin_borrow [lexical] [[X_MOVEONLY]]
// CHECK:   [[X_COPY:%.*]] = explicit_copy_value [[X]]
// CHECK:   [[X_MOVEONLYWRAPPED_MARKED:%.*]] = mark_must_check [no_implicit_copy] [[X_COPY]]
// CHECK:   [[BORROWED_X_MOVEONLYWRAPPED_MARKED_1:%.*]] = begin_borrow [[X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[BORROWED_X_MOVEONLYWRAPPED_MARKED_2:%.*]] = begin_borrow [[X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[FUNC:%.*]] = function_ref @$sSi1poiyS2i_SitFZ : $@convention(method) (Int, Int, @thin Int.Type) -> Int // user: %15
// CHECK:   [[GUARANTEED_ESCAPED_X_1:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MOVEONLYWRAPPED_MARKED_1]]
// CHECK:   [[GUARANTEED_ESCAPED_X_2:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MOVEONLYWRAPPED_MARKED_2]]
// CHECK:   apply [[FUNC]]([[GUARANTEED_ESCAPED_X_1]], [[GUARANTEED_ESCAPED_X_2]], {{.*}})
// CHECK:   [[BORROWED_X_MOVEONLYWRAPPED_MARKED:%.*]] = begin_borrow [[X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   [[GUARANTEED_ESCAPED_X:%.*]] = moveonlywrapper_to_copyable [guaranteed] [[BORROWED_X_MOVEONLYWRAPPED_MARKED]]
// CHECK:   store [[GUARANTEED_ESCAPED_X]] to [trivial] {{%.*}} : $*Int
// CHECK: } // end sil function '$s14noimplicitcopy8printIntyyF'
func printInt() {
    @_noImplicitCopy let x: Int = 5
    print2(x + x)
    print("printInt: \(x)")
}

// NOTE: MOW expands to MOVEONLYWRAPPED
//
// CHECK-LABEL: sil hidden [ossa] @$s14noimplicitcopy10printKlassyyF : $@convention(thin) () -> () {
// CHECK:   [[X:%.*]] = begin_borrow [lexical] {{%[0-9]+}} : $Klass
// CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
// CHECK:   [[X_MOVEONLYWRAPPED:%.*]] = copyable_to_moveonlywrapper [[X_COPY]]
// CHECK:   [[X_MOVEONLYWRAPPED_MARKED:%.*]] = mark_must_check [no_implicit_copy] [[X_MOVEONLYWRAPPED]]
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
func printKlass() {
    @_noImplicitCopy let x = Klass()
    x.increment()
    x.increment()
    print("printKlass: \(x)")
    print("printKlass: \(x.i)")
}
