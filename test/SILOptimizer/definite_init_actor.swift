// RUN: %target-swift-frontend -module-name test -target %target-swift-5.1-abi-triple -swift-version 5 -sil-verify-all -Xllvm -sil-print-types -emit-sil %s | %FileCheck --enable-var-scope --implicit-check-not='hop_to_executor' %s

// REQUIRES: concurrency
// REQUIRES: swift_in_compiler

 enum ActingError<T> : Error {
   case forgotLine
   case smuggledValue(T)
 }

func neverReturn() -> Never { fatalError("quit!") }
func arbitraryAsync() async {}

actor BoringActor {

    // CHECK-LABEL: sil hidden @$s4test11BoringActorCACyYacfc : $@convention(method) @async (@sil_isolated @owned BoringActor) -> @owned BoringActor {
    // CHECK:   bb0([[SELF:%[0-9]+]] : $BoringActor):
    // CHECK:       initializeDefaultActor
    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK-NEXT:  hop_to_executor [[EI]]
    // CHECK: } // end sil function '$s4test11BoringActorCACyYacfc'
    init() async {}

    // CHECK-LABEL: sil hidden @$s4test11BoringActorC4sizeACSi_tYacfc : $@convention(method) @async (Int, @sil_isolated @owned BoringActor) -> @owned BoringActor {
    // CHECK:   bb0({{%[0-9]+}} : $Int, [[SELF:%[0-9]+]] : $BoringActor):
    // CHECK:       initializeDefaultActor
    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK-NEXT:  hop_to_executor [[EI]]
    // CHECK: } // end sil function '$s4test11BoringActorC4sizeACSi_tYacfc'
    init(size: Int) async {
        var sz = size
        while sz > 0 {
            print(".")
            sz -= 1
        }
        print("done!")
    }

    // CHECK-LABEL: sil hidden @$s4test11BoringActorC04mainC0ACyt_tYacfc : $@convention(method) @async (@owned BoringActor) -> @owned BoringActor {
    // CHECK:         builtin "initializeDefaultActor"
    // CHECK:         [[FN:%[0-9]+]] = function_ref @$s4test14arbitraryAsyncyyYaF : $@convention(thin) @async () -> ()
    // CHECK:         = apply [[FN]]() : $@convention(thin) @async () -> ()
    //   TODO: We should be able to eliminate this hop because it's immediately
    //     prior to the return.
    // CHECK-NEXT:    hop_to_executor {{%[0-9]+}} : $MainActor
    @MainActor
    init(mainActor: Void) async {
      await arbitraryAsync()
    }

    // CHECK-LABEL: sil hidden @$s4test11BoringActorC6crashyACSgyt_tYacfc : $@convention(method) @async (@sil_isolated @owned BoringActor) -> @owned Optional<BoringActor> {
    // CHECK:   bb0([[SELF:%[0-9]+]] : $BoringActor):
    // CHECK:       initializeDefaultActor
    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK-NEXT:  hop_to_executor [[EI]]
    // CHECK: } // end sil function '$s4test11BoringActorC6crashyACSgyt_tYacfc'
    init!(crashy: Void) async { return nil }

    // CHECK-LABEL: sil hidden @$s4test11BoringActorC5nillyACSgSi_tYacfc : $@convention(method) @async (Int, @sil_isolated @owned BoringActor) -> @owned Optional<BoringActor> {
    // CHECK:   bb0({{%[0-9]+}} : $Int, [[SELF:%[0-9]+]] : $BoringActor):
    // CHECK:       initializeDefaultActor
    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK-NEXT:  hop_to_executor [[EI]]
    // CHECK: } // end sil function '$s4test11BoringActorC5nillyACSgSi_tYacfc'
    init?(nilly: Int) async {
        guard nilly > 0 else { return nil }
    }
}

 actor SingleVarActor {
    var myVar: Int

    init(sync: Void) {
      myVar = 0
    }

    // CHECK-LABEL: sil hidden @$s4test14SingleVarActorCACyYacfc : $@convention(method) @async (@sil_isolated @owned SingleVarActor) -> @owned SingleVarActor {
    // CHECK:    bb0([[SELF:%[0-9]+]] : $SingleVarActor):
    //   TODO: We should be able to eliminate this by reasoning that the stores
    //     are to local memory.
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]
    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK:       store {{%[0-9]+}} to [[ACCESS:%[0-9]+]]
    // CHECK-NEXT:  end_access [[ACCESS]]
    // CHECK-NEXT:  hop_to_executor [[EI]] : $SingleVarActor
    // CHECK:       store {{%[0-9]+}} to {{%[0-9]+}}
    // CHECK: } // end sil function '$s4test14SingleVarActorCACyYacfc'
    init() async {
        myVar = 0
        myVar = 1
    }

    // CHECK-LABEL: sil hidden @$s4test14SingleVarActorC10iterationsACSi_tYacfc : $@convention(method) @async (Int, @sil_isolated @owned SingleVarActor) -> @owned SingleVarActor {
    // CHECK:   bb0({{%[0-9]+}} : $Int, [[SELF:%[0-9]+]] : $SingleVarActor):
    //   TODO: We should be able to eliminate this by reasoning that the stores
    //     are to local memory.
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]
    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK:       [[MYVAR_REF:%[0-9]+]] = ref_element_addr [[EI]] : $SingleVarActor, #SingleVarActor.myVar
    // CHECK:       [[MYVAR:%[0-9]+]] = begin_access [init] [static] [[MYVAR_REF]] : $*Int
    // CHECK:       store {{%[0-9]+}} to [[MYVAR]] : $*Int
    // CHECK-NEXT:  end_access [[MYVAR]]
    // CHECK-NEXT:  hop_to_executor [[EI]] : $SingleVarActor
    // CHECK: } // end sil function '$s4test14SingleVarActorC10iterationsACSi_tYacfc'
    init(iterations: Int) async {
        var iter = iterations
        repeat {
            myVar = 0
            iter -= 1
        } while iter > 0
    }

    // CHECK-LABEL: sil hidden @$s4test14SingleVarActorC2b12b2ACSb_SbtYacfc : $@convention(method) @async (Bool, Bool, @sil_isolated @owned SingleVarActor) -> @owned SingleVarActor {
    // CHECK:   bb0({{%[0-9]+}} : $Bool, {{%[0-9]+}} : $Bool, [[SELF:%[0-9]+]] : $SingleVarActor):

    //   TODO: We should be able to eliminate this by reasoning that the stores
    //     are to local memory.
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]

    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK:       store {{%[0-9]+}} to [[A1:%[0-9]+]] : $*Int
    // CHECK-NEXT:  end_access [[A1]]
    // CHECK-NEXT:  hop_to_executor [[EI]] : $SingleVarActor

    // CHECK:       store {{%[0-9]+}} to [[A2:%[0-9]+]] : $*Int
    // CHECK-NEXT:  end_access [[A2]]
    // CHECK-NEXT:  hop_to_executor [[EI]] : $SingleVarActor

    // CHECK:       store {{%[0-9]+}} to [[A3:%[0-9]+]] : $*Int
    // CHECK-NEXT:  end_access [[A3]]
    // CHECK-NEXT:  hop_to_executor [[EI]] : $SingleVarActor

    // CHECK: } // end sil function '$s4test14SingleVarActorC2b12b2ACSb_SbtYacfc'
    init(b1: Bool, b2: Bool) async {
        if b1 {
            if b2 {
                myVar = 0
            }
            myVar = 1
        }
        myVar = 2
    }

   // CHECK-LABEL: sil hidden @$s4test14SingleVarActorC14failable_asyncACSgSb_tYacfc : $@convention(method) @async (Bool, @sil_isolated @owned SingleVarActor) -> @owned Optional<SingleVarActor> {
   // CHECK: bb0({{%[0-9]+}} : $Bool, {{%[0-9]+}} : $SingleVarActor):

   //   TODO: We should be able to eliminate this by reasoning that the stores
   //     are to local memory.
   // CHECK:   [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
   // CHECK:   hop_to_executor [[GENERIC_EXECUTOR]]

   // CHECK:   cond_br {{%[0-9]+}}, [[SUCCESS_BB:bb[0-9]+]], {{bb[0-9]+}}
   //
   // CHECK: [[SUCCESS_BB]]:
   // CHECK:   store {{%[0-9]+}} to {{%[0-9]+}} : $*Int
   // CHECK:   hop_to_executor {{%[0-9]+}}
   // CHECK:   enum $Optional<SingleVarActor>, #Optional.some!enumelt
   //
   // CHECK: } // end sil function '$s4test14SingleVarActorC14failable_asyncACSgSb_tYacfc'
   init?(failable_async cond: Bool) async {
     guard cond else { return nil }
     myVar = 1
   }

   // CHECK-LABEL: sil hidden @$s4test14SingleVarActorC10delegatingACSb_tYacfC : $@convention(method) @async (Bool, @thick SingleVarActor.Type) -> @owned SingleVarActor {
   // CHECK:         [[SELF_ALLOC:%[0-9]+]] = alloc_stack [lexical] [var_decl] $SingleVarActor, let, name "self"

   //   Initial hop to the generic executor.
   // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
   // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]

   //   Hop immediately after the call to the synchronous init.
   // CHECK:         [[SYNC_FN:%[0-9]+]] = function_ref @$s4test14SingleVarActorC4syncACyt_tcfC : $@convention(method) (@thick SingleVarActor.Type) -> @owned SingleVarActor
   // CHECK:         [[INIT1:%[0-9]+]] = apply [[SYNC_FN]]({{%[0-9]+}}) : $@convention(method) (@thick SingleVarActor.Type) -> @owned SingleVarActor
   // CHECK:         store [[INIT1]] to [[SELF_ALLOC]] : $*SingleVarActor
   // CHECK-NEXT:    hop_to_executor [[INIT1]] : $SingleVarActor

   //   Hop immediately after the call to the async init.
   // CHECK:         [[ASYNC_FN:%[0-9]+]] = function_ref @$s4test14SingleVarActorCACyYacfC : $@convention(method) @async (@thick SingleVarActor.Type) -> @owned SingleVarActor
   // CHECK:         [[INIT2:%[0-9]+]] = apply [[ASYNC_FN]]({{%[0-9]+}}) : $@convention(method) @async (@thick SingleVarActor.Type) -> @owned SingleVarActor
   //   TODO: SILGen emits a hop to the current executor after this async
   //     call, and apparently we don't reliably clean it up.
   // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<any Actor>, #Optional.none
   // CHECK-NEXT:    hop_to_executor [[GENERIC_EXECUTOR]]
   // CHECK:         store [[INIT2]] to [[SELF_ALLOC]] : $*SingleVarActor
   // CHECK-NEXT:    hop_to_executor [[INIT2]] : $SingleVarActor

   // CHECK:         bb3([[T0:%.*]] : $SingleVarActor):
   // CHECK:         [[ARBITRARY_FN:%.*]] = function_ref @$s4test14arbitraryAsyncyyYaF
   // CHECK-NEXT:    apply [[ARBITRARY_FN]]()
   // CHECK-NEXT:    strong_retain [[T0]]
   // CHECK-NEXT:    [[T1:%.*]] = init_existential_ref [[T0]] :
   // CHECK-NEXT:    [[T2:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[T1]] :
   // CHECK-NEXT:    hop_to_executor [[T2]] :
   // CHECK-NEXT:    release_value [[T2]] :

   // CHECK:       } // end sil function '$s4test14SingleVarActorC10delegatingACSb_tYacfC'
   convenience init(delegating c: Bool) async {
     if c {
       self.init(sync: ())
     } else {
       await self.init()
     }
     await arbitraryAsync()
   }

 }

actor DefaultInit {
    var x: DefaultInit?
    var y: String = ""
    var z: ActingError<Int> = .smuggledValue(5)

    // CHECK-LABEL: sil hidden @$s4test11DefaultInitCACyYacfc : $@convention(method) @async (@sil_isolated @owned DefaultInit) -> @owned DefaultInit {
    // CHECK:   bb0([[SELF:%[0-9]+]] : $DefaultInit):
    //   Initial hop to the generic executor.
    //   TODO: This should be easy to remove.
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]
    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK:       store {{%[0-9]+}} to {{%[0-9]+}} : $*ActingError<Int>
    // CHECK-NEXT:  hop_to_executor [[EI]] : $DefaultInit
    // CHECK: } // end sil function '$s4test11DefaultInitCACyYacfc'
    init() async {}

    // CHECK-LABEL: sil hidden @$s4test11DefaultInitC5nillyACSgSb_tYacfc : $@convention(method) @async (Bool, @sil_isolated @owned DefaultInit) -> @owned Optional<DefaultInit> {
    // CHECK:   bb0({{%[0-9]+}} : $Bool, [[SELF:%[0-9]+]] : $DefaultInit):
    //   Initial hop to the generic executor.
    //   TODO: This should be fairly easy to remove.
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]
    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK:       store {{%[0-9]+}} to {{%[0-9]+}} : $*ActingError<Int>
    // CHECK-NEXT:  hop_to_executor [[EI]] : $DefaultInit
    // CHECK: } // end sil function '$s4test11DefaultInitC5nillyACSgSb_tYacfc'
    init?(nilly: Bool) async {
        guard nilly else { return nil }
    }

    init(sync: Void) {}
    @MainActor init(mainActorSync: Void) {}
}

actor MultiVarActor {
    var firstVar: Int
    var secondVar: Float

    // CHECK-LABEL: sil hidden @$s4test13MultiVarActorC10doNotThrowACSb_tYaKcfc : $@convention(method) @async (Bool, @sil_isolated @owned MultiVarActor) -> (@owned MultiVarActor, @error any Error) {
    // CHECK:   bb0({{%[0-9]+}} : $Bool, [[SELF:%[0-9]+]] : $MultiVarActor):
    //   Initial hop to the generic executor.
    //   TODO: This should be easy to remove.
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]
    // CHECK:       [[EI:%.*]] = end_init_let_ref [[SELF]]
    // CHECK:       [[REF:%[0-9]+]] = ref_element_addr [[EI]] : $MultiVarActor, #MultiVarActor.firstVar
    // CHECK:       [[VAR:%[0-9]+]] = begin_access [init] [static] [[REF]] : $*Int
    // CHECK:       store {{%[0-9]+}} to [[VAR]] : $*Int
    // CHECK-NEXT:  end_access [[VAR]]
    // CHECK-NEXT:  hop_to_executor [[EI]] : $MultiVarActor
    // CHECK: } // end sil function '$s4test13MultiVarActorC10doNotThrowACSb_tYaKcfc'
    init(doNotThrow: Bool) async throws {
        secondVar = 0
        guard doNotThrow else { throw ActingError<Any>.forgotLine }
        firstVar = 1
    }

    // CHECK-LABEL: sil hidden @$s4test13MultiVarActorC10noSuccCaseACSb_tYacfc : $@convention(method) @async (Bool, @sil_isolated @owned MultiVarActor) -> @owned MultiVarActor {
    //   Initial hop to the generic executor.
    //   TODO: This should be easy to remove.
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]

    // CHECK:       store {{%[0-9]+}} to [[A1:%[0-9]+]] : $*Int
    // CHECK-NEXT:  end_access [[A1]]
    // CHECK-NEXT:  hop_to_executor {{%[0-9]+}} : $MultiVarActor

    // CHECK:       store {{%[0-9]+}} to [[A2:%[0-9]+]] : $*Int
    // CHECK-NEXT:  end_access [[A2]]
    // CHECK-NEXT:  hop_to_executor {{%[0-9]+}} : $MultiVarActor
    // CHECK: } // end sil function '$s4test13MultiVarActorC10noSuccCaseACSb_tYacfc'
    init(noSuccCase: Bool) async {
        secondVar = 0
        if noSuccCase {
            firstVar = 1
        }
        firstVar = 2
    }

    // CHECK-LABEL: sil hidden @$s4test13MultiVarActorC10noPredCaseACSb_tYacfc : $@convention(method) @async (Bool, @sil_isolated @owned MultiVarActor) -> @owned MultiVarActor {
    //   Initial hop to the generic executor.
    //   TODO: This should be easy to remove.
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]

    // CHECK:       store {{%[0-9]+}} to [[ACCESS:%[0-9]+]] : $*Int
    // CHECK-NEXT:  end_access [[ACCESS]]
    // CHECK-NEXT:  hop_to_executor {{%[0-9]+}} : $MultiVarActor
    // CHECK: } // end sil function '$s4test13MultiVarActorC10noPredCaseACSb_tYacfc'
    init(noPredCase: Bool) async {
        secondVar = 0
        firstVar = 1
        if noPredCase {
            print("hello")
        }
        print("hi")
    }

    // These don't fully-initialize `self`, so DI won't add a hop, but we still
    // have the initial hop emitted by SILGen.

    // CHECK-LABEL: sil hidden @$s4test13MultiVarActorC17doesNotFullyInit1ACSgSb_tYacfc
    //   TODO: We should be able to remove this hop by proving that all the
    //     stores before return are local.  We don't really care about invalid
    //     code, of course, but it should fall out.
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]
    init?(doesNotFullyInit1: Bool) async {
        firstVar = 1
        return nil
    }

    // CHECK-LABEL: sil hidden @$s4test13MultiVarActorC17doesNotFullyInit2ACSb_tYacfc
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]
    init(doesNotFullyInit2: Bool) async {
        firstVar = 1
        fatalError("I give up!")
    }

    // CHECK-LABEL: sil hidden @$s4test13MultiVarActorC17doesNotFullyInit3ACSb_tYaKcfc
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]
    init(doesNotFullyInit3: Bool) async throws {
        firstVar = 1
        throw ActingError<Any>.forgotLine
    }

    // CHECK-LABEL: sil hidden @$s4test13MultiVarActorC17doesNotFullyInit4ACSgSb_tYacfc
    // CHECK:       [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:  hop_to_executor [[GENERIC_EXECUTOR]]
    init?(doesNotFullyInit4: Bool) async {
        firstVar = 1
        neverReturn()
    }
}

@available(SwiftStdlib 5.1, *)
actor TaskMaster {
    var task: Task<Void, Never>?

    func sayHello() { print("hello") }

    ////// for the initializer
    // CHECK-LABEL: @$s4test10TaskMasterCACyYacfc : $@convention(method) @async (@sil_isolated @owned TaskMaster) -> @owned TaskMaster {
    //   Initial hop to the generic executor.
    //   TODO: This should be easy to remove.
    // CHECK:           [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:      hop_to_executor [[GENERIC_EXECUTOR]]

    // CHECK:           [[ELM:%[0-9]+]] = ref_element_addr [[SELF:%[0-9]+]] : $TaskMaster, #TaskMaster.task
    // CHECK:           [[NIL:%[0-9]+]] = enum $Optional<Task<(), Never>>, #Optional.none!enumelt
    // CHECK:           store [[NIL]] to [[ELM]] : $*Optional<Task<(), Never>>
    // CHECK-NEXT:      hop_to_executor [[SELF]] : $TaskMaster
    // CHECK: } // end sil function '$s4test10TaskMasterCACyYacfc'
    init() async {

        ////// for the closure
        // CHECK-LABEL: sil private @$s4test10TaskMasterCACyYacfcyyYacfU_ :
        // CHECK:           hop_to_executor {{%[0-9]+}} : $TaskMaster
        // CHECK: } // end sil function '$s4test10TaskMasterCACyYacfcyyYacfU_'
        task = Task.detached { await self.sayHello() }
    }
}

actor SomeActor {
    var x: Int = 0

    // CHECK-LABEL: sil hidden @$s4test9SomeActorCACyYacfc : $@convention(method) @async (@sil_isolated @owned SomeActor) -> @owned SomeActor {
    //   Initial hop to the generic executor.
    //   TODO: This should be easy to remove.
    // CHECK:           [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK-NEXT:      hop_to_executor [[GENERIC_EXECUTOR]]

    // CHECK-NOT:       begin_access
    // CHECK:           store {{%[0-9]+}} to {{%[0-9]+}} : $*Int
    // CHECK-NEXT:      hop_to_executor {{%[0-9]+}} : $SomeActor
    // CHECK: } // end sil function '$s4test9SomeActorCACyYacfc'
    init() async {}
}

actor Ahmad {
  var x: Int = 0
  
  // CHECK-LABEL: sil hidden @$s4test5AhmadCACyYacfc : $@convention(method) @async (@owned Ahmad) -> @owned Ahmad {
  // CHECK:         bb0{{.*}}:
  // CHECK-NEXT:      [[GENERIC:%[0-9]+]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
  // CHECK-NEXT:      hop_to_executor [[GENERIC]]
  // CHECK:           store {{%[0-9]+}} to {{%[0-9]+}} : $*Int
  // CHECK: } // end sil function '$s4test5AhmadCACyYacfc'
  nonisolated init() async {}
}
