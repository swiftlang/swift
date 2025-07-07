// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-copy-propagation=requested-passes-only -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module | %FileCheck %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -enable-copy-propagation=requested-passes-only -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module | %FileCheck -check-prefix=CHECK-SIL %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -enable-copy-propagation=requested-passes-only -module-name moveonly -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -O -Xllvm -sil-disable-pass=FunctionSignatureOpts | %FileCheck -check-prefix=CHECK-SIL-OPT %s

// REQUIRES: swift_in_compiler

import Swift

class Klass {}

// CHECK-LABEL: sil [ossa] @$s8moveonly7useCopyyAA5KlassCADF : {{.*}} {
// CHECK:       bb0([[ARG:%.*]] :
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[EXPLICIT_COPY:%[^,]+]] = explicit_copy_value [[ARG]]
// CHECK-NEXT:    return [[EXPLICIT_COPY]]
// CHECK-LABEL: } // end sil function '$s8moveonly7useCopyyAA5KlassCADF'

// CHECK-SIL-LABEL: sil @$s8moveonly7useCopyyAA5KlassCADF : {{.*}} {
// CHECK-SIL:       bb0([[ARG:%.*]] : $Klass):
// CHECK-SIL-NEXT:    debug_value
// CHECK-SIL-NEXT:    strong_retain [[ARG]]
// CHECK-SIL-NEXT:    return [[ARG]] : $Klass
// CHECK-SIL-LABEL: } // end sil function '$s8moveonly7useCopyyAA5KlassCADF'

// CHECK-SIL-OPT-LABEL: sil {{.*}}@$s8moveonly7useCopyyAA5KlassCADF : {{.*}} {
// CHECK-SIL-OPT:       bb0([[ARG:%.*]] : $Klass):
// CHECK-SIL-OPT-NEXT:    debug_value
// CHECK-SIL-OPT-NEXT:    strong_retain [[ARG]]
// CHECK-SIL-OPT-NEXT:    return [[ARG]]
// CHECK-SIL-OPT-LABEL: } // end sil function '$s8moveonly7useCopyyAA5KlassCADF'
public func useCopy(_ k: Klass) -> Klass {
    copy k
}

// CHECK-LABEL: sil [ossa] @$s8moveonly7useCopyyxxRlzClF : $@convention(thin) <T where T : AnyObject> (@guaranteed T) -> @owned T {
// CHECK:       bb0([[ARG:%.*]] :
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[EXPLICIT_COPY:%[^,]+]] = explicit_copy_value [[ARG]]
// CHECK-NEXT:    return [[EXPLICIT_COPY]]
// CHECK-LABEL: } // end sil function '$s8moveonly7useCopyyxxRlzClF'

// CHECK-SIL-LABEL: sil @$s8moveonly7useCopyyxxRlzClF : {{.*}} {
// CHECK-SIL:       bb0([[ARG:%.*]] :
// CHECK-SIL-NEXT:    debug_value
// CHECK-SIL-NEXT:    strong_retain [[ARG]]
// CHECK-SIL-NEXT:    return [[ARG]]
// CHECK-SIL-LABEL: } // end sil function '$s8moveonly7useCopyyxxRlzClF'

// CHECK-SIL-OPT-LABEL: sil {{.*}}@$s8moveonly7useCopyyxxRlzClF : {{.*}} {
// CHECK-SIL-OPT:       bb0([[ARG:%.*]] :
// CHECK-SIL-OPT-NEXT:    debug_value
// CHECK-SIL-OPT-NEXT:    strong_retain [[ARG]]
// CHECK-SIL-OPT-NEXT:    return [[ARG]] : $T
// CHECK-SIL-OPT-LABEL: } // end sil function '$s8moveonly7useCopyyxxRlzClF'
public func useCopy<T : AnyObject>(_ k: T) -> T {
    copy k
}
