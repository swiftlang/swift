// RUN: %target-swift-emit-silgen -I %t  -disable-availability-checking -strict-concurrency=complete -enable-experimental-feature IsolatedDefaultValues -parse-as-library %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: asserts

@MainActor
func requiresMainActor() -> Int { 0 }

@MainActor
func mainActorDefaultArg(value: Int = requiresMainActor()) {}

// CHECK-LABEL: sil hidden [ossa] @$s30isolated_default_argument_eval15mainActorCalleryyF
@MainActor func mainActorCaller() {
  mainActorDefaultArg()
}

// CHECK-LABEL: sil hidden [ossa] @$s30isolated_default_argument_eval22nonisolatedAsyncCalleryyYaF
func nonisolatedAsyncCaller() async {
  // CHECK: hop_to_executor {{.*}} : $Optional<Builtin.Executor>
  // CHECK: [[GETARG:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval19mainActorDefaultArg5valueySi_tFfA_
  // CHECK: hop_to_executor {{.*}} : $MainActor
  // CHECK-NEXT: apply [[GETARG]]()
  // CHECK-NEXT: hop_to_executor {{.*}} : $Optional<Builtin.Executor>
  await mainActorDefaultArg()
}
