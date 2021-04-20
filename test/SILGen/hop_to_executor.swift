// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency | %FileCheck --enable-var-scope %s
// REQUIRES: concurrency


actor MyActor {

  private var p: Int

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC6calleeyySiYaF : $@convention(method) @async (Int, @guaranteed MyActor) -> () {
  // CHECK-NOT:   hop_to_executor
  // CHECK:     } // end sil function '$s4test7MyActorC6calleeyySiYaF'
  nonisolated func callee(_ x: Int) async {
    print(x)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC14throwingCalleeyySiYaKF : $@convention(method) @async (Int, @guaranteed MyActor) -> @error Error {
  // CHECK-NOT:   hop_to_executor{{ }}
  // CHECK:     } // end sil function '$s4test7MyActorC14throwingCalleeyySiYaKF'
  nonisolated func throwingCallee(_ x: Int) async throws {
    print(x)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC0A13AsyncFunctionyyYaKF : $@convention(method) @async (@guaranteed MyActor) -> @error Error {
  // CHECK:        hop_to_executor %0 : $MyActor 
  // CHECK:        = apply {{.*}} : $@convention(method) @async (Int, @guaranteed MyActor) -> ()
  // CHECK-NEXT:   hop_to_executor %0 : $MyActor 
  // CHECK:        try_apply {{.*}}, normal bb1, error bb2
  // CHECK:      bb1({{.*}}):
  // CHECK-NEXT:   hop_to_executor %0 : $MyActor 
  // CHECK:      bb2({{.*}}):
  // CHECK-NEXT:   hop_to_executor %0 : $MyActor 
  // CHECK:      } // end sil function '$s4test7MyActorC0A13AsyncFunctionyyYaKF'
  func testAsyncFunction() async throws {
    await callee(p)
    try await throwingCallee(p)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC0A22ConsumingAsyncFunctionyyYaF : $@convention(method) @async (@owned MyActor) -> () {
  // CHECK:        [[BORROWED_SELF:%[0-9]+]] = begin_borrow %0 : $MyActor
  // CHECK:        hop_to_executor [[BORROWED_SELF]] : $MyActor 
  // CHECK:        = apply {{.*}} : $@convention(method) @async (Int, @guaranteed MyActor) -> ()
  // CHECK-NEXT:   hop_to_executor [[BORROWED_SELF]] : $MyActor 
  // CHECK:      } // end sil function '$s4test7MyActorC0A22ConsumingAsyncFunctionyyYaF'
  __consuming func testConsumingAsyncFunction() async {
    await callee(p)
  }


  // CHECK-LABEL: sil private [ossa] @$s4test7MyActorC0A7ClosureSiyYaFSiyYaXEfU_ : $@convention(thin) @async (@guaranteed MyActor) -> Int {
  // CHECK:        [[COPIED_SELF:%[0-9]+]] = copy_value %0 : $MyActor
  // CHECK:        [[BORROWED_SELF:%[0-9]+]] = begin_borrow [[COPIED_SELF]] : $MyActor
  // CHECK:        hop_to_executor [[BORROWED_SELF]] : $MyActor 
  // CHECK:        = apply
  // CHECK:      } // end sil function '$s4test7MyActorC0A7ClosureSiyYaFSiyYaXEfU_'
  func testClosure() async -> Int {
    return await { () async in p }()
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC13dontInsertHTESiyF : $@convention(method) (@guaranteed MyActor) -> Int {
  // CHECK-NOT:   hop_to_executor
  // CHECK:     } // end sil function '$s4test7MyActorC13dontInsertHTESiyF'
  func dontInsertHTE() -> Int {
    return p
  }

  init() {
    p = 27
  }
}
  
@globalActor
struct GlobalActor {
  static var shared: MyActor = MyActor()
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A11GlobalActoryyYaF : $@convention(thin) @async () -> () {
// CHECK:   [[F:%[0-9]+]] = function_ref @$s4test11GlobalActorV6sharedAA02MyC0Cvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[P:%[0-9]+]] = apply [[F]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[A:%[0-9]+]] = pointer_to_address %2 : $Builtin.RawPointer to [strict] $*MyActor
// CHECK:   [[ACC:%[0-9]+]] = begin_access [read] [dynamic] [[A]] : $*MyActor
// CHECK:   [[L:%[0-9]+]] = load [copy] [[ACC]] : $*MyActor
// CHECK:   [[B:%[0-9]+]] = begin_borrow [[L]] : $MyActor
// CHECK:   hop_to_executor [[B]] : $MyActor
// CHECK: } // end sil function '$s4test0A11GlobalActoryyYaF'
@GlobalActor
func testGlobalActor() async {
}

// CHECK-LABEL: sil private [ossa] @$s4test0A22GlobalActorWithClosureyyYaFyyYaXEfU_ : $@convention(thin) @async () -> () {
// CHECK:   [[F:%[0-9]+]] = function_ref @$s4test11GlobalActorV6sharedAA02MyC0Cvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[P:%[0-9]+]] = apply [[F]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[A:%[0-9]+]] = pointer_to_address %2 : $Builtin.RawPointer to [strict] $*MyActor
// CHECK:   [[ACC:%[0-9]+]] = begin_access [read] [dynamic] [[A]] : $*MyActor
// CHECK:   [[L:%[0-9]+]] = load [copy] [[ACC]] : $*MyActor
// CHECK:   [[B:%[0-9]+]] = begin_borrow [[L]] : $MyActor
// CHECK:   hop_to_executor [[B]] : $MyActor
// CHECK: } // end sil function '$s4test0A22GlobalActorWithClosureyyYaFyyYaXEfU_'
@GlobalActor
func testGlobalActorWithClosure() async {
  await { () async in }()
}

@globalActor
struct GenericGlobalActorWithGetter<T> {
  static var shared: MyActor { return MyActor() }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A28GenericGlobalActorWithGetteryyYaF : $@convention(thin) @async () -> () {
// CHECK:     [[MT:%[0-9]+]] = metatype $@thin GenericGlobalActorWithGetter<Int>.Type
// CHECK:     [[F:%[0-9]+]] = function_ref @$s4test28GenericGlobalActorWithGetterV6sharedAA02MyD0CvgZ : $@convention(method) <τ_0_0> (@thin GenericGlobalActorWithGetter<τ_0_0>.Type) -> @owned MyActor
// CHECK:     [[A:%[0-9]+]] = apply [[F]]<Int>([[MT]]) : $@convention(method) <τ_0_0> (@thin GenericGlobalActorWithGetter<τ_0_0>.Type) -> @owned MyActor
// CHECK:     [[B:%[0-9]+]] = begin_borrow [[A]] : $MyActor
// CHECK:     hop_to_executor [[B]] : $MyActor
// CHECK: } // end sil function '$s4test0A28GenericGlobalActorWithGetteryyYaF'
@GenericGlobalActorWithGetter<Int>
func testGenericGlobalActorWithGetter() async {
}


actor RedActorImpl {
  // CHECK-LABEL: sil hidden [ossa] @$s4test12RedActorImplC5helloyySiF : $@convention(method) (Int, @guaranteed RedActorImpl) -> () {
  // CHECK-NOT: hop_to_executor
  // CHECK: } // end sil function '$s4test12RedActorImplC5helloyySiF'
  func hello(_ x : Int) {}
}

actor BlueActorImpl {
// CHECK-LABEL: sil hidden [ossa] @$s4test13BlueActorImplC4poke6personyAA03RedcD0C_tYaF : $@convention(method) @async (@guaranteed RedActorImpl, @guaranteed BlueActorImpl) -> () {
// CHECK:       bb0([[RED:%[0-9]+]] : @guaranteed $RedActorImpl, [[BLUE:%[0-9]+]] : @guaranteed $BlueActorImpl):
// CHECK:         hop_to_executor [[BLUE]] : $BlueActorImpl
// CHECK-NOT:     hop_to_executor
// CHECK:         [[INTARG:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NOT:     hop_to_executor
// CHECK:         [[METH:%[0-9]+]] = class_method [[RED]] : $RedActorImpl, #RedActorImpl.hello : (RedActorImpl) -> (Int) -> (), $@convention(method) (Int, @guaranteed RedActorImpl) -> ()
// CHECK:         hop_to_executor [[RED]] : $RedActorImpl
// CHECK-NEXT:    {{%[0-9]+}} = apply [[METH]]([[INTARG]], [[RED]]) : $@convention(method) (Int, @guaranteed RedActorImpl) -> ()
// CHECK-NEXT:    hop_to_executor [[BLUE]] : $BlueActorImpl
// CHECK-NOT:     hop_to_executor
// CHECK: } // end sil function '$s4test13BlueActorImplC4poke6personyAA03RedcD0C_tYaF'
  func poke(person red : RedActorImpl) async {
    await red.hello(42)
  }

// CHECK-LABEL: sil hidden [ossa] @$s4test13BlueActorImplC14createAndGreetyyYaF : $@convention(method) @async (@guaranteed BlueActorImpl) -> () {
// CHECK:     bb0([[BLUE:%[0-9]+]] : @guaranteed $BlueActorImpl):
// CHECK:       hop_to_executor [[BLUE]] : $BlueActorImpl
// CHECK:       [[RED:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(method) (@thick RedActorImpl.Type) -> @owned RedActorImpl
// CHECK:       [[REDBORROW:%[0-9]+]] = begin_borrow [[RED]] : $RedActorImpl
// CHECK:       [[INTARG:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       [[METH:%[0-9]+]] = class_method [[REDBORROW]] : $RedActorImpl, #RedActorImpl.hello : (RedActorImpl) -> (Int) -> (), $@convention(method) (Int, @guaranteed RedActorImpl) -> ()
// CHECK:       hop_to_executor [[REDBORROW]] : $RedActorImpl
// CHECK-NEXT:  = apply [[METH]]([[INTARG]], [[REDBORROW]]) : $@convention(method) (Int, @guaranteed RedActorImpl) -> ()
// CHECK-NEXT:  hop_to_executor [[BLUE]] : $BlueActorImpl
// CHECK:       end_borrow [[REDBORROW]] : $RedActorImpl
// CHECK:       destroy_value [[RED]] : $RedActorImpl
// CHECK: } // end sil function '$s4test13BlueActorImplC14createAndGreetyyYaF'
  func createAndGreet() async {
    let red = RedActorImpl()  // <- key difference from `poke` is local construction of the actor
    await red.hello(42)
  }
}

@globalActor
struct RedActor {
  static var shared: RedActorImpl { RedActorImpl() }
}

@globalActor
struct BlueActor {
  static var shared: BlueActorImpl { BlueActorImpl() }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test5redFnyySiF : $@convention(thin) (Int) -> () {
// CHECK-NOT: hop_to_executor{{ }}
// CHECK: } // end sil function '$s4test5redFnyySiF'
@RedActor func redFn(_ x : Int) {}

// CHECK-LABEL: sil hidden [ossa] @$s4test6blueFnyyYaF : $@convention(thin) @async () -> () {
      // ---- switch to blue actor, since we're an async function ----
// CHECK:       [[MT:%[0-9]+]] = metatype $@thin BlueActor.Type
// CHECK:       [[F:%[0-9]+]] = function_ref @$s4test9BlueActorV6sharedAA0bC4ImplCvgZ : $@convention(method) (@thin BlueActor.Type) -> @owned BlueActorImpl
// CHECK:       [[B:%[0-9]+]] = apply [[F]]([[MT]]) : $@convention(method) (@thin BlueActor.Type) -> @owned BlueActorImpl
// CHECK:       [[BLUEEXE:%[0-9]+]] = begin_borrow [[B]] : $BlueActorImpl
// CHECK:       hop_to_executor [[BLUEEXE]] : $BlueActorImpl
      // ---- evaluate the argument to redFn ----
// CHECK:       [[LIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 100
// CHECK:       [[INTMT:%[0-9]+]] = metatype $@thin Int.Type
// CHECK:       [[CTOR:%[0-9]+]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       [[ARG:%[0-9]+]] = apply [[CTOR]]([[LIT]], [[INTMT]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
      // ---- prepare to invoke redFn ----
// CHECK:       [[CALLEE:%[0-9]+]] = function_ref @$s4test5redFnyySiF : $@convention(thin) (Int) -> ()
      // ---- obtain and hop to RedActor's executor ----
// CHECK:       [[REDMT:%[0-9]+]] = metatype $@thin RedActor.Type
// CHECK:       [[GETTER:%[0-9]+]] = function_ref @$s4test8RedActorV6sharedAA0bC4ImplCvgZ : $@convention(method) (@thin RedActor.Type) -> @owned RedActorImpl
// CHECK:       [[R:%[0-9]+]] = apply [[GETTER]]([[REDMT]]) : $@convention(method) (@thin RedActor.Type) -> @owned RedActorImpl
// CHECK:       [[REDEXE:%[0-9]+]] = begin_borrow [[R]] : $RedActorImpl
// CHECK:       hop_to_executor [[REDEXE]] : $RedActorImpl
      // ---- now invoke redFn, hop back to Blue, and clean-up ----
// CHECK-NEXT:  {{%[0-9]+}} = apply [[CALLEE]]([[ARG]]) : $@convention(thin) (Int) -> ()
// CHECK-NEXT:  hop_to_executor [[BLUEEXE]] : $BlueActorImpl
// CHECK:       end_borrow [[REDEXE]] : $RedActorImpl
// CHECK:       destroy_value [[R]] : $RedActorImpl
// CHECK:       end_borrow [[BLUEEXE]] : $BlueActorImpl
// CHECK:       destroy_value [[B]] : $BlueActorImpl
// CHECK-NOT:     hop_to_executor
// CHECK: } // end sil function '$s4test6blueFnyyYaF'
@BlueActor func blueFn() async {
  await redFn(100)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test20unspecifiedAsyncFuncyyYaF : $@convention(thin) @async () -> () {
// CHECK-NOT:     hop_to_executor
// CHECK:         [[BORROW:%[0-9]+]] = begin_borrow {{%[0-9]+}} : $RedActorImpl
// CHECK-NEXT:    [[PREV_EXEC:%.*]] = builtin "getCurrentExecutor"()
// CHECK-NEXT:    hop_to_executor [[BORROW]] : $RedActorImpl
// CHECK-NEXT:    {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(thin) (Int) -> ()
// CHECK-NEXT:    hop_to_executor [[PREV_EXEC]]
// CHECK-NEXT:    end_borrow [[BORROW]] : $RedActorImpl
// CHECK-NOT:     hop_to_executor
// CHECK: } // end sil function '$s4test20unspecifiedAsyncFuncyyYaF'
func unspecifiedAsyncFunc() async {
  await redFn(200)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test27anotherUnspecifiedAsyncFuncyyAA12RedActorImplCYaF : $@convention(thin) @async (@guaranteed RedActorImpl) -> () {
// CHECK:       bb0([[RED:%[0-9]+]] : @guaranteed $RedActorImpl):
// CHECK-NOT:     hop_to_executor
// CHECK:         [[INTARG:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NOT:     hop_to_executor
// CHECK:         [[METH:%[0-9]+]] = class_method [[RED]] : $RedActorImpl, #RedActorImpl.hello : (RedActorImpl) -> (Int) -> (), $@convention(method) (Int, @guaranteed RedActorImpl) -> ()
// CHECK-NEXT:    [[PREV_EXEC:%.*]] = builtin "getCurrentExecutor"()
// CHECK-NEXT:    hop_to_executor [[RED]] : $RedActorImpl
// CHECK-NEXT:    {{%[0-9]+}} = apply [[METH]]([[INTARG]], [[RED]]) : $@convention(method) (Int, @guaranteed RedActorImpl) -> ()
// CHECK-NEXT:    hop_to_executor [[PREV_EXEC]]
// CHECK: } // end sil function '$s4test27anotherUnspecifiedAsyncFuncyyAA12RedActorImplCYaF'
func anotherUnspecifiedAsyncFunc(_ red : RedActorImpl) async {
  await red.hello(12);
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A20GlobalActorFuncValueyyyyXEYaF
// CHECK: function_ref @$s4test8RedActorV6sharedAA0bC4ImplCvgZ
// CHECK: hop_to_executor [[RED:%[0-9]+]] : $RedActorImpl
// CHECK-NEXT: apply
// CHECK-NEXT: hop_to_executor [[PREV:%[0-9]+]] : $Builtin.Executor
func testGlobalActorFuncValue(_ fn: @RedActor () -> Void) async {
  await fn()
}
