// RUN: %target-swift-emit-silgen -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module | %FileCheck %s

import Swift

class Klass {}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly7useMoveyAA5KlassCADF : $@convention(thin) (@guaranteed Klass) -> @owned Klass {
// CHECK: bb0([[ARG:%.*]] :
// CHECK-NEXT: debug_value
// CHECK-NEXT: copy_value
// CHECK-NEXT: move_value
// CHECK-NEXT: return
// CHECK: } // end sil function '$s8moveonly7useMoveyAA5KlassCADF'
func useMove(_ k: Klass) -> Klass {
    Builtin.move(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly7useMoveyxxRlzClF : $@convention(thin) <T where T : AnyObject> (@guaranteed T) -> @owned T {
// CHECK: bb0([[ARG:%.*]] :
// CHECK-NEXT: debug_value
// CHECK-NEXT: copy_value
// CHECK-NEXT: move_value
// CHECK-NEXT: return
// CHECK: } // end sil function '$s8moveonly7useMoveyxxRlzClF'
func useMove<T : AnyObject>(_ k: T) -> T {
    Builtin.move(k)
}
