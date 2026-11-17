// RUN: %target-swift-emit-silgen -enable-sil-opaque-values -enable-builtin-module %s | %FileCheck %s

import Builtin

// CHECK-LABEL: sil hidden [ossa] [opaque] @{{.*}}taskRunInlineIgnored{{.*}} : $@convention(thin) <T> (@guaranteed @noescape @async @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>) -> () {
// CHECK:         [[RESULT:%.*]] = builtin "taskRunInline"<T>(%0) : $T
// CHECK-NEXT:    ignored_use [[RESULT]]
// CHECK-NEXT:    destroy_value [[RESULT]]
// CHECK-LABEL: } // end sil function '{{.*}}taskRunInlineIgnored{{.*}}'
func taskRunInlineIgnored<T>(_ body: () async -> T) {
  _ = Builtin.taskRunInline(body)
}
