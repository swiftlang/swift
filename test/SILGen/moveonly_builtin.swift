// RUN: %target-swift-emit-silgen -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-experimental-move-only | %FileCheck %s
// RUN: %target-swift-emit-sil -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-experimental-move-only | %FileCheck -check-prefix=CHECK-SIL %s

// REQUIRES: rdar84780237

import Swift

class Klass {}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly7useMoveyAA5KlassCADF : $@convention(thin) (@guaranteed Klass) -> @owned Klass {
// CHECK: bb0([[ARG:%.*]] :
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = alloc_stack $Klass
// CHECK-NEXT: [[ARC_COPY:%.*]] = copy_value [[ARG]]
// CHECK-NEXT: [[INPUT_ADDR:%.*]] = alloc_stack $Klass
// CHECK-NEXT: store [[ARC_COPY]] to [init] [[INPUT_ADDR]]
// CHECK-NEXT: // function_ref _move<A>(_:)
// CHECK-NEXT: [[MOVE:%.*]] = function_ref @$ss5_moveyxxnlF : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
// CHECK-NEXT: [[APPLY_RESULT:%.*]] = apply [[MOVE]]<Klass>([[RESULT_ADDR]], [[INPUT_ADDR]])
// CHECK-NEXT: dealloc_stack [[INPUT_ADDR]]
// CHECK-NEXT: [[RELOADED_VALUE:%.*]] = load [take] [[RESULT_ADDR]]
// CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
// CHECK-NEXT: return [[RELOADED_VALUE]]
// CHECK: } // end sil function '$s8moveonly7useMoveyAA5KlassCADF'

// CHECK-SIL-LABEL: sil hidden @$s8moveonly7useMoveyAA5KlassCADF : $@convention(thin) (@guaranteed Klass) -> @owned Klass {
// CHECK-SIL: bb0([[ARG:%.*]] :
// CHECK-SIL-NEXT: debug_value
// CHECK-SIL-NEXT: strong_retain
// CHECK-SIL-NEXT: [allows_diagnostics] move_value
// CHECK-SIL-NEXT: tuple
// CHECK-SIL-NEXT: return
// CHECK-SIL: } // end sil function '$s8moveonly7useMoveyAA5KlassCADF'
func useMove(_ k: Klass) -> Klass {
    _move(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly7useMoveyxxRlzClF : $@convention(thin) <T where T : AnyObject> (@guaranteed T) -> @owned T {
// CHECK: bb0([[ARG:%.*]] :
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = alloc_stack $T
// CHECK-NEXT: [[ARC_COPY:%.*]] = copy_value [[ARG]]
// CHECK-NEXT: [[INPUT_ADDR:%.*]] = alloc_stack $T
// CHECK-NEXT: store [[ARC_COPY]] to [init] [[INPUT_ADDR]]
// CHECK-NEXT: // function_ref _move<A>(_:)
// CHECK-NEXT: [[MOVE:%.*]] = function_ref @$ss5_moveyxxnlF : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
// CHECK-NEXT: [[APPLY_RESULT:%.*]] = apply [[MOVE]]<T>([[RESULT_ADDR]], [[INPUT_ADDR]])
// CHECK-NEXT: dealloc_stack [[INPUT_ADDR]]
// CHECK-NEXT: [[RELOADED_VALUE:%.*]] = load [take] [[RESULT_ADDR]]
// CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
// CHECK-NEXT: return [[RELOADED_VALUE]]
// CHECK: } // end sil function '$s8moveonly7useMoveyxxRlzClF'

// CHECK-SIL-LABEL: sil hidden @$s8moveonly7useMoveyxxRlzClF : $@convention(thin) <T where T : AnyObject> (@guaranteed T) -> @owned T {
// CHECK-SIL: bb0([[ARG:%.*]] :
// CHECK-SIL-NEXT: debug_value
// CHECK-SIL-NEXT: strong_retain
// CHECK-SIL-NEXT: [allows_diagnostics] move_value
// CHECK-SIL-NEXT: tuple
// CHECK-SIL-NEXT: return
// CHECK-SIL: } // end sil function '$s8moveonly7useMoveyxxRlzClF'
func useMove<T : AnyObject>(_ k: T) -> T {
    _move(k)
}
