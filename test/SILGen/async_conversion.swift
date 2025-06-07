// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5  -target %target-swift-5.1-abi-triple | %FileCheck --implicit-check-not hop_to_executor %s
// REQUIRES: concurrency

func f(_: Int, _: String) -> String? { nil }

// CHECK-LABEL: sil hidden [ossa] @$s4testAAyyF : $@convention(thin) () -> () {
func test() {
  // CHECK: [[F:%.*]] = function_ref @$s4test1fySSSgSi_SStF : $@convention(thin) (Int, @guaranteed String) -> @owned Optional<String>
  // CHECK: [[THICK_F:%.*]] = thin_to_thick_function [[F]] : $@convention(thin) (Int, @guaranteed String) -> @owned Optional<String> to $@callee_guaranteed (Int, @guaranteed String) -> @owned Optional<String>
  // CHECK: [[THUNK:%.*]] = function_ref @$sSiS2SSgIegygo_SiSSAAIegHygo_TR : $@convention(thin) @async (Int, @guaranteed String, @guaranteed @callee_guaranteed (Int, @guaranteed String) -> @owned Optional<String>) -> @owned Optional<String>
  // CHECK: partial_apply [callee_guaranteed] [[THUNK]]([[THICK_F]]) : $@convention(thin) @async (Int, @guaranteed String, @guaranteed @callee_guaranteed (Int, @guaranteed String) -> @owned Optional<String>) -> @owned Optional<String>
  let _: (Int, String) async -> String? = f
}

protocol P {
  func f(_: Int, _: String) async -> String?
}

struct X: P {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test1XVAA1PA2aDP1fySSSgSi_SStYaFTW : $@convention(witness_method: P) @async (Int, @guaranteed String, @in_guaranteed X) -> @owned Optional<String>
  // CHECK: function_ref @$s4test1XV1fySSSgSi_SStF : $@convention(method) (Int, @guaranteed String, X) -> @owned Optional<String>
  func f(_: Int, _: String) -> String? { nil }
}

protocol P2 {
  init() async
}

actor A: P2 {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test1ACAA2P2A2aDPxyYacfCTW
  // CHECK-NOT: hop_to_executor
  // CHECK: function_ref @$s4test1ACACycfC : $@convention(method) (@thick A.Type) -> @owned A // user: %3
  // CHECK-NEXT: apply
  // CHECK: return
}

// It's important that async thunks generated for conversions
// from a nonisolated, non-async function to an async function
// do _not_ contain hops to any executor, including the generic executor (!!),
// because these specific async functions derived from non-async ones
// must inherit their executor to make conversion sequences such as these, safe:

@MainActor func mainActorFn() {}

// CHECK-LABEL: sil hidden [ossa] @$s4test7caller1yyYaF : $@convention(thin) @async () -> () {
@MainActor func caller1() async {
  // CHECK: hop_to_executor {{.*}} : $MainActor
  // CHECK: [[F:%.*]] = function_ref @$s4test11mainActorFnyyF : $@convention(thin) () -> ()
  // CHECK: [[THICK_F:%.*]] = thin_to_thick_function [[F]] : $@convention(thin) () -> () to $@callee_guaranteed () -> ()
  // CHECK: [[THICK_F_VAR:%.*]] = move_value [lexical] [var_decl] [[THICK_F]]
  // CHECK: [[THICK_F_BORROW:%.*]] = begin_borrow [[THICK_F_VAR]]
  // CHECK: [[THICK_F_COPY:%.*]] = copy_value [[THICK_F_BORROW]]
  // CHECK: [[THUNK:%.*]] = function_ref @$sIeg_IegH_TR : $@convention(thin) @async (@guaranteed @callee_guaranteed () -> ()) -> ()
  // CHECK:  = partial_apply [callee_guaranteed] [[THUNK]]([[THICK_F_COPY]]) : $@convention(thin) @async (@guaranteed @callee_guaranteed () -> ()) -> ()
  // ... after applying ...
  // CHECK: hop_to_executor {{.*}} : $MainActor
  let f: () -> () = mainActorFn
  let g: () async -> () = f
  await g()  // g cannot be hopping to a different executor, it must inherit!
}
// CHECK: end sil function '$s4test7caller1yyYaF'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIeg_IegH_TR : $@convention(thin) @async (@guaranteed @callee_guaranteed () -> ()) -> () {
// CHECK-NOT: hop_to_executor
// CHECK: end sil function '$sIeg_IegH_TR'


actor AnActor {
  func isolatedMethod(_ i: Int) {}

  // CHECK-LABEL: sil hidden [ossa] @$s4test7AnActorC6calleryyYaF : $@convention(method) @async (@sil_isolated @guaranteed AnActor) -> () {
  func caller() async {
    // CHECK: hop_to_executor {{..*}} : $AnActor
    //  [[F:%.*]] = function_ref @$s4test7AnActorC6calleryyYaFySicACYicfu_ : $@convention(thin) (@guaranteed AnActor) -> @owned @callee_guaranteed (Int) -> ()
    //  [[APPLIED_F:%.*]] = apply [[F]]({{.*}}) : $@convention(thin) (@guaranteed AnActor) -> @owned @callee_guaranteed (Int) -> ()
    //  [[BORROWED_F:%.*]] = begin_borrow [lexical] [var_decl] [[APPLIED_F]] : $@callee_guaranteed (Int) -> ()
    //  [[COPIED_F:%.*]] = copy_value [[BORROWED_F]] : $@callee_guaranteed (Int) -> ()
    //  [[THUNK:%.*]] = function_ref @$sSiIegy_SiIegHy_TR : $@convention(thin) @async (Int, @guaranteed @callee_guaranteed (Int) -> ()) -> ()
    //  = partial_apply [callee_guaranteed] [[THUNK]]([[COPIED_F]]) : $@convention(thin) @async (Int, @guaranteed @callee_guaranteed (Int) -> ()) -> ()
    // ... after applying ...
    // CHECK: hop_to_executor {{.*}} : $AnActor
    let f: (Int) -> () = self.isolatedMethod
    let g: (Int) async -> () = f
    await g(0)
  }
} // CHECK: end sil function '$s4test7AnActorC6calleryyYaF'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSiIegy_SiIegHy_TR : $@convention(thin) @async (Int, @guaranteed @callee_guaranteed (Int) -> ()) -> () {
// CHECK-NOT: hop_to_executor
// CHECK: end sil function '$sSiIegy_SiIegHy_TR'

