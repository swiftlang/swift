// RUN: %target-swift-emit-silgen -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-experimental-move-only | %FileCheck %s
// RUN: %target-swift-emit-sil -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-experimental-move-only | %FileCheck -check-prefix=CHECK-SIL %s

// REQUIRES: optimized_stdlib

import Swift

class Klass {}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly7useMoveyAA5KlassCADnF : $@convention(thin) (@owned Klass) -> @owned Klass {
// CHECK: bb0([[ARG:%.*]] :
// CHECK-NEXT: [[BORROWED_ARG:%.*]] = begin_borrow [lexical] [[ARG]]
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = alloc_stack $Klass
// CHECK-NEXT: [[ARC_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK-NEXT: [[INPUT_ADDR:%.*]] = alloc_stack $Klass
// CHECK-NEXT: store [[ARC_COPY]] to [init] [[INPUT_ADDR]]
// CHECK-NEXT: // function_ref _move<A>(_:)
// CHECK-NEXT: [[MOVE:%.*]] = function_ref @$ss5_moveyxxnlF : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
// CHECK-NEXT: [[APPLY_RESULT:%.*]] = apply [[MOVE]]<Klass>([[RESULT_ADDR]], [[INPUT_ADDR]])
// CHECK-NEXT: dealloc_stack [[INPUT_ADDR]]
// CHECK-NEXT: [[RELOADED_VALUE:%.*]] = load [take] [[RESULT_ADDR]]
// CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
// CHECK-NEXT: end_borrow [[BORROWED_ARG]]
// CHECK-NEXT: destroy_value [[ARG]]
// CHECK-NEXT: return [[RELOADED_VALUE]]
// CHECK: } // end sil function '$s8moveonly7useMoveyAA5KlassCADnF'

// CHECK-SIL-LABEL: sil hidden @$s8moveonly7useMoveyAA5KlassCADnF : $@convention(thin) (@owned Klass) -> @owned Klass {
// CHECK-SIL: bb0([[ARG:%.*]] :
// CHECK-SIL-NEXT: debug_value [moved]
// CHECK-SIL-NEXT: strong_retain
// CHECK-SIL-NEXT: move_value
// CHECK-SIL-NEXT: debug_value [moved] undef
// CHECK-SIL-NEXT: tuple
// CHECK-SIL-NEXT: tuple
// CHECK-SIL-NEXT: strong_release
// CHECK-SIL-NEXT: return
// CHECK-SIL: } // end sil function '$s8moveonly7useMoveyAA5KlassCADnF'
func useMove(_ k: __owned Klass) -> Klass {
    _move(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly7useMoveyxxnRlzClF : $@convention(thin) <T where T : AnyObject> (@owned T) -> @owned T {
// CHECK: bb0([[ARG:%.*]] :
// CHECK-NEXT: [[BORROWED_ARG:%.*]] = begin_borrow [lexical] [[ARG]]
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = alloc_stack $T
// CHECK-NEXT: [[ARC_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK-NEXT: [[INPUT_ADDR:%.*]] = alloc_stack $T
// CHECK-NEXT: store [[ARC_COPY]] to [init] [[INPUT_ADDR]]
// CHECK-NEXT: // function_ref _move<A>(_:)
// CHECK-NEXT: [[MOVE:%.*]] = function_ref @$ss5_moveyxxnlF : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
// CHECK-NEXT: [[APPLY_RESULT:%.*]] = apply [[MOVE]]<T>([[RESULT_ADDR]], [[INPUT_ADDR]])
// CHECK-NEXT: dealloc_stack [[INPUT_ADDR]]
// CHECK-NEXT: [[RELOADED_VALUE:%.*]] = load [take] [[RESULT_ADDR]]
// CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
// CHECK-NEXT: end_borrow [[BORROWED_ARG]]
// CHECK-NEXT: destroy_value [[ARG]]
// CHECK-NEXT: return [[RELOADED_VALUE]]
// CHECK: } // end sil function '$s8moveonly7useMoveyxxnRlzClF'

// CHECK-SIL-LABEL: sil hidden @$s8moveonly7useMoveyxxnRlzClF : $@convention(thin) <T where T : AnyObject> (@owned T) -> @owned T {
// CHECK-SIL: bb0([[ARG:%.*]] :
// CHECK-SIL-NEXT: debug_value [moved]
// CHECK-SIL-NEXT: strong_retain
// CHECK-SIL-NEXT: move_value
// CHECK-SIL-NEXT: debug_value [moved] undef
// CHECK-SIL-NEXT: tuple
// CHECK-SIL-NEXT: tuple
// CHECK-SIL-NEXT: strong_release
// CHECK-SIL-NEXT: return
// CHECK-SIL: } // end sil function '$s8moveonly7useMoveyxxnRlzClF'
func useMove<T : AnyObject>(_ k: __owned T) -> T {
    _move(k)
}
