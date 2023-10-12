// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5  -disable-availability-checking | %FileCheck --enable-var-scope %s --implicit-check-not 'hop_to_executor {{%[0-9]+}}' --dump-input=always
// REQUIRES: concurrency

///// testClosure()
// CHECK-LABEL: sil hidden [ossa] @$s4test0A7ClosureSiyYaF : $@convention(thin) @async () -> Int {
// CHECK:         [[GET_EXEC_FN:%[0-9]+]] = function_ref @swift_task_getPreferredTaskExecutor : $@convention(thin) () -> Builtin.Executor
// CHECK:         [[EXEC:%[0-9]+]] = apply [[GET_EXEC_FN]]() : $@convention(thin) () -> Builtin.Executor
// CHECK:         [[EXEC_O:%[0-9]+]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[EXEC]] : $Builtin.Executor
// CHECK:         hop_to_executor [[EXEC_O]] : $Optional<Builtin.Executor>
// Call the closure
// CHECK:         [[CLOSURE_REF:%[0-9]+]] = function_ref @$s4test0A7ClosureSiyYaFSiyYaXEfU_ : $@convention(thin) @async () -> Int
// CHECK:         [[CLOSURE_RET:%[0-9]+]] = apply [[CLOSURE_REF]]() : $@convention(thin) @async () -> Int
// We need to hop back to our expected executor:
// CHECK:         hop_to_executor [[EXEC_O]] : $Optional<Builtin.Executor>
// Return the result
// CHECK:         return [[CLOSURE_RET]] : $Int
// CHECK:       } // end sil function '$s4test0A7ClosureSiyYaF'

///// closure #1 in testClosure()
// CHECK-LABEL: sil private [ossa] @$s4test0A7ClosureSiyYaFSiyYaXEfU_ : $@convention(thin) @async () -> Int {
// CHECK:         [[GET_EXEC_FN:%[0-9]+]] = function_ref @swift_task_getPreferredTaskExecutor : $@convention(thin) () -> Builtin.Executor
// CHECK:         [[EXEC:%[0-9]+]] = apply [[GET_EXEC_FN]]() : $@convention(thin) () -> Builtin.Executor
// CHECK:         [[EXEC_O:%[0-9]+]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[EXEC]] : $Builtin.Executor
// CHECK:         hop_to_executor [[EXEC_O]] : $Optional<Builtin.Executor>
//                ...
// CHECK:       } // end sil function '$s4test0A7ClosureSiyYaFSiyYaXEfU_'
func testClosure() async -> Int {
  return await { () async in 13 }()
}
