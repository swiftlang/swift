// RUN: %target-swift-emit-silgen -enable-copy-propagation=requested-passes-only -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-experimental-move-only | %FileCheck %s
// RUN: %target-swift-emit-sil -enable-copy-propagation=requested-passes-only -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-experimental-move-only | %FileCheck -check-prefix=CHECK-SIL %s
// RUN: %target-swift-emit-sil -enable-copy-propagation=requested-passes-only -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -O -Xllvm -sil-disable-pass=FunctionSignatureOpts -enable-experimental-move-only | %FileCheck -check-prefix=CHECK-SIL-OPT %s

import Swift

class Klass {}

// NOTE:
//
// One will notice in these tests that we are not promoting away the input
// argument to _copy. This is because PredictableMemOpts does not know how to
// handle store_borrow. With time, that will happen and this unfortunateness
// will go away. At -O this problem will not exist though.
//
// That being said, this is actually not a big issue for the usage of this in
// dataflow based OSSA checking since the store_borrow will always just be
// treated as a +0 use of any owned value we are tracking, so it will not be
// treated as a lifetime ending use (albeit it would be considered an
// escaping use).

// CHECK-LABEL: sil [ossa] @$s8moveonly7useCopyyAA5KlassCADF : $@convention(thin) (@guaranteed Klass) -> @owned Klass {
// CHECK: bb0([[ARG:%.*]] :
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = alloc_stack $Klass
// CHECK-NEXT: [[INPUT_ADDR:%.*]] = alloc_stack $Klass
// CHECK-NEXT: [[SB:%.*]] = store_borrow [[ARG]] to [[INPUT_ADDR]]
// CHECK-NEXT: // function_ref _copy<A>(_:)
// CHECK-NEXT: [[COPY:%.*]] = function_ref @$ss5_copyyxxlF :
// CHECK-NEXT: [[APPLY_RESULT:%.*]] = apply [[COPY]]<Klass>([[RESULT_ADDR]], [[SB]])
// CHECK-NEXT: end_borrow
// CHECK-NEXT: dealloc_stack [[INPUT_ADDR]]
// CHECK-NEXT: [[RELOADED_VALUE:%.*]] = load [take] [[RESULT_ADDR]]
// CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
// CHECK-NEXT: return [[RELOADED_VALUE]]
// CHECK: } // end sil function '$s8moveonly7useCopyyAA5KlassCADF'

// CHECK-SIL-LABEL: sil @$s8moveonly7useCopyyAA5KlassCADF : $@convention(thin) (@guaranteed Klass) -> @owned Klass {
// CHECK-SIL: bb0([[ARG:%.*]] : $Klass):
// CHECK-SIL-NEXT: debug_value
// CHECK-SIL-NEXT: [[INPUT:%.*]] = alloc_stack [lexical] $Klass
// CHECK-SIL-NEXT: store [[ARG]] to [[INPUT]]
// CHECK-SIL-NEXT: [[VALUE:%[0-9][0-9]*]] = load [[INPUT]]{{.*}}
// CHECK-SIL-NEXT: strong_retain [[VALUE]]
// CHECK-SIL-NEXT: strong_retain [[VALUE]]
// CHECK-SIL-NEXT: strong_retain [[VALUE]]
// CHECK-SIL-NEXT: strong_release [[VALUE]]
// CHECK-SIL-NEXT: tuple ()
// CHECK-SIL-NEXT: dealloc_stack [[INPUT]] : $*Klass
// CHECK-SIL-NEXT: strong_release [[VALUE]] : $Klass
// CHECK-SIL-NEXT: return [[VALUE]] : $Klass
// CHECK-SIL: } // end sil function '$s8moveonly7useCopyyAA5KlassCADF'

// CHECK-SIL-OPT-LABEL: sil {{.*}}@$s8moveonly7useCopyyAA5KlassCADF : $@convention(thin) (@guaranteed Klass) -> @owned Klass {
// CHECK-SIL-OPT: bb0([[ARG:%.*]] : $Klass):
// CHECK-SIL-OPT-NEXT: debug_value
// CHECK-SIL-OPT-NEXT: strong_retain [[ARG]]
// CHECK-SIL-OPT-NEXT: return [[ARG]]
// CHECK-SIL-OPT: } // end sil function '$s8moveonly7useCopyyAA5KlassCADF'
public func useCopy(_ k: Klass) -> Klass {
    _copy(k)
}

// CHECK-LABEL: sil [ossa] @$s8moveonly7useCopyyxxRlzClF : $@convention(thin) <T where T : AnyObject> (@guaranteed T) -> @owned T {
// CHECK: bb0([[ARG:%.*]] :
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = alloc_stack $T
// CHECK-NEXT: [[INPUT_ADDR:%.*]] = alloc_stack $T
// CHECK-NEXT: [[SB:%.*]] = store_borrow [[ARG]] to [[INPUT_ADDR]]
// CHECK-NEXT: // function_ref _copy<A>(_:)
// CHECK-NEXT: [[COPY:%.*]] = function_ref @$ss5_copyyxxlF :
// CHECK-NEXT: [[APPLY_RESULT:%.*]] = apply [[COPY]]<T>([[RESULT_ADDR]], [[SB]])
// CHECK-NEXT: end_borrow
// CHECK-NEXT: dealloc_stack [[INPUT_ADDR]]
// CHECK-NEXT: [[RELOADED_VALUE:%.*]] = load [take] [[RESULT_ADDR]]
// CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
// CHECK-NEXT: return [[RELOADED_VALUE]]
// CHECK: } // end sil function '$s8moveonly7useCopyyxxRlzClF'

// CHECK-SIL-LABEL: sil @$s8moveonly7useCopyyxxRlzClF : $@convention(thin) <T where T : AnyObject> (@guaranteed T) -> @owned T {
// CHECK-SIL: bb0([[ARG:%.*]] :
// CHECK-SIL-NEXT: debug_value
// CHECK-SIL-NEXT: [[INPUT_TO_BE_ELIMINATED:%.*]] = alloc_stack [lexical] $T
// CHECK-SIL-NEXT: store [[ARG]] to [[INPUT]] : $*T
// CHECK-SIL-NEXT: [[VALUE:%.*]] = load [[INPUT]] : $*T
// CHECK-SIL-NEXT: strong_retain [[VALUE]]
// CHECK-SIL-NEXT: strong_retain [[VALUE]]
// CHECK-SIL-NEXT: strong_retain [[VALUE]]
// CHECK-SIL-NEXT: strong_release [[VALUE]]
// CHECK-SIL-NEXT: tuple ()
// CHECK-SIL-NEXT: dealloc_stack [[INPUT]]
// CHECK-SIL-NEXT: strong_release [[VALUE]] : $T
// CHECK-SIL-NEXT: return [[VALUE]] : $T
// CHECK-SIL: } // end sil function '$s8moveonly7useCopyyxxRlzClF'

// CHECK-SIL-OPT-LABEL: sil {{.*}}@$s8moveonly7useCopyyxxRlzClF : $@convention(thin) <T where T : AnyObject> (@guaranteed T) -> @owned T {
// CHECK-SIL-OPT: bb0([[ARG:%.*]] :
// CHECK-SIL-OPT-NEXT: debug_value
// CHECK-SIL-OPT-NEXT: strong_retain [[ARG]]
// CHECK-SIL-OPT-NEXT: return [[ARG]] : $T
// CHECK-SIL-OPT: } // end sil function '$s8moveonly7useCopyyxxRlzClF'
public func useCopy<T : AnyObject>(_ k: T) -> T {
    _copy(k)
}
