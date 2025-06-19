// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5  -target %target-swift-5.1-abi-triple | %FileCheck --enable-var-scope %s --implicit-check-not 'hop_to_executor {{%[0-9]+}}'
// REQUIRES: concurrency

// CHECK-LABEL: sil hidden [ossa] @$s4test16unspecifiedAsyncyyYaF : $@convention(thin) @async () -> ()
// CHECK:         bb0:
// CHECK-NEXT:      [[GENERIC:%[0-9]+]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
// CHECK-NEXT:      hop_to_executor [[GENERIC]]
// CHECK:       } // end sil function '$s4test16unspecifiedAsyncyyYaF'
func unspecifiedAsync() async {}

actor MyActor {

  private var p: Int

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC6calleeyySiYaF : $@convention(method) @async (Int, @guaranteed MyActor) -> () {
  // CHECK:       [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
  // CHECK:       hop_to_executor [[GENERIC_EXEC]] :
  // CHECK:     } // end sil function '$s4test7MyActorC6calleeyySiYaF'
  nonisolated func callee(_ x: Int) async {
    print(x)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC14throwingCalleeyySiYaKF : $@convention(method) @async (Int, @guaranteed MyActor) -> @error any Error {
  // CHECK:       [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
  // CHECK:       hop_to_executor [[GENERIC_EXEC]] :
  // CHECK:     } // end sil function '$s4test7MyActorC14throwingCalleeyySiYaKF'
  nonisolated func throwingCallee(_ x: Int) async throws {
    print(x)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC0A13AsyncFunctionyyYaKF : $@convention(method) @async (@sil_isolated @guaranteed MyActor) -> @error any Error {
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

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC0A22ConsumingAsyncFunctionyyYaF : $@convention(method) @async (@sil_isolated @owned MyActor) -> () {
  // CHECK:        [[BORROWED_SELF:%[0-9]+]] = begin_borrow %0 : $MyActor
  // CHECK:        hop_to_executor [[BORROWED_SELF]] : $MyActor 
  // CHECK:        = apply {{.*}} : $@convention(method) @async (Int, @guaranteed MyActor) -> ()
  // CHECK:        hop_to_executor [[BORROWED_SELF]] : $MyActor
  // CHECK:      } // end sil function '$s4test7MyActorC0A22ConsumingAsyncFunctionyyYaF'
  __consuming func testConsumingAsyncFunction() async {
    await callee(p)
  }

  ///// MyActor.testClosure()
  // CHECK: sil hidden [ossa] @$s4test7MyActorC0A7ClosureSiyYaF : $@convention(method) @async (@sil_isolated @guaranteed MyActor) -> Int {
  // CHECK:     hop_to_executor [[A:%[0-9]+]] : $MyActor
  // CHECK:     {{%[0-9]+}} = apply
  // CHECK:     hop_to_executor [[A]] : $MyActor
  // CHECK: } // end sil function '$s4test7MyActorC0A7ClosureSiyYaF'

  ///// closure #1 in MyActor.testClosure()
  // CHECK-LABEL: sil private [ossa] @$s4test7MyActorC0A7ClosureSiyYaFSiyYaXEfU_ : $@convention(thin) @async (@sil_isolated @guaranteed MyActor) -> Int {
  // CHECK:        [[COPIED_SELF:%[0-9]+]] = copy_value %0 : $MyActor
  // CHECK:        [[BORROWED_SELF:%[0-9]+]] = begin_borrow [[COPIED_SELF]] : $MyActor
  // CHECK:        hop_to_executor [[BORROWED_SELF]] : $MyActor 
  // CHECK:        = apply
  // CHECK:      } // end sil function '$s4test7MyActorC0A7ClosureSiyYaFSiyYaXEfU_'
  func testClosure() async -> Int {
    return await { () async in p }()
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC13dontInsertHTESiyF : $@convention(method) (@sil_isolated @guaranteed MyActor) -> Int {
  // CHECK-NOT:   hop_to_executor
  // CHECK:     } // end sil function '$s4test7MyActorC13dontInsertHTESiyF'
  func dontInsertHTE() -> Int {
    return p
  }

  init() {
    p = 27
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC4withACSi_tYacfc : $@convention(method) @async (Int, @owned MyActor) -> @owned MyActor {
  // CHECK:         bb0({{%[0-9]+}} : $Int, {{%[0-9]+}} : @owned $MyActor):
  // CHECK:           hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:           builtin "initializeDefaultActor"
  // CHECK:           [[FUNC:%[0-9]+]] = function_ref @$s4test16unspecifiedAsyncyyYaF : $@convention(thin) @async () -> ()
  // CHECK:           %24 = apply [[FUNC]]() : $@convention(thin) @async () -> ()
  // CHECK-NEXT:      hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:       } // end sil function '$s4test7MyActorC4withACSi_tYacfc'
  @MainActor
  init(with v: Int) async {
    p = 1
    await unspecifiedAsync()
    p = 2
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC10delegatingACSb_tYacfC : $@convention(method) @async (Bool, @thick MyActor.Type) -> @owned MyActor {
  //        ** first hop is in the prologue
  // CHECK:         bb0({{%[0-9]+}} : $Bool, {{%[0-9]+}} : $@thick MyActor.Type):
  // CHECK:           hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:           struct_extract {{%[0-9]+}} : $Bool, #Bool._value
  // CHECK:           cond_br
  //        ** second hop is right after delegating to the async init
  // CHECK:         = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) @async (Int, @thick MyActor.Type) -> @owned MyActor
  // CHECK-NEXT:    hop_to_executor {{%[0-9]+}} : $MainActor
  //        ** third hop is right after calling an arbitrary async function
  // CHECK:         [[FUNC:%[0-9]+]] = function_ref @$s4test16unspecifiedAsyncyyYaF : $@convention(thin) @async () -> ()
  // CHECK:         = apply [[FUNC]]() : $@convention(thin) @async () -> ()
  // CHECK-NEXT:    hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:       } // end sil function '$s4test7MyActorC10delegatingACSb_tYacfC'
  @MainActor
  convenience init(delegating c: Bool) async {
    if c {
      self.init()
    } else {
      await self.init(with: 0)
    }
    await unspecifiedAsync()
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

// CHECK-LABEL: sil hidden [ossa] @$s4test0A17GlobalActorUnsafeyyYaF : $@convention(thin) @async () -> () {
// CHECK:   [[F:%[0-9]+]] = function_ref @$s4test11GlobalActorV6sharedAA02MyC0Cvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[P:%[0-9]+]] = apply [[F]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[A:%[0-9]+]] = pointer_to_address %2 : $Builtin.RawPointer to [strict] $*MyActor
// CHECK:   [[ACC:%[0-9]+]] = begin_access [read] [dynamic] [[A]] : $*MyActor
// CHECK:   [[L:%[0-9]+]] = load [copy] [[ACC]] : $*MyActor
// CHECK:   [[B:%[0-9]+]] = begin_borrow [[L]] : $MyActor
// CHECK:   hop_to_executor [[B]] : $MyActor
// CHECK: }
@GlobalActor
@preconcurrency
func testGlobalActorUnsafe() async {
}

///// testGlobalActorWithClosure()
// CHECK: sil hidden [ossa] @$s4test0A22GlobalActorWithClosureyyYaF : $@convention(thin) @async () -> () {
// CHECK:     hop_to_executor [[A:%[0-9]+]] : $MyActor
// CHECK:     {{%[0-9]+}} = apply
// CHECK:     hop_to_executor [[A]] : $MyActor
// CHECK: // end sil function '$s4test0A22GlobalActorWithClosureyyYaF'

///// closure #1 in testGlobalActorWithClosure()
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

actor MyGenericActor<T> {}

@globalActor
struct GenericGlobalActorWithGetter<T> {
  static var shared: MyGenericActor<T> { return MyGenericActor() }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A28GenericGlobalActorWithGetteryyYaF : $@convention(thin) @async () -> () {
// CHECK:     [[MT:%[0-9]+]] = metatype $@thin GenericGlobalActorWithGetter<Int>.Type
// CHECK:     [[F:%[0-9]+]] = function_ref @$s4test28GenericGlobalActorWithGetterV6sharedAA02MybD0CyxGvgZ
// CHECK:     [[A:%[0-9]+]] = apply [[F]]<Int>([[MT]])
// CHECK:     [[B:%[0-9]+]] = begin_borrow [[A]] : $MyGenericActor<Int>
// CHECK:     hop_to_executor [[B]] : $MyGenericActor<Int>
// CHECK: } // end sil function '$s4test0A28GenericGlobalActorWithGetteryyYaF'
@GenericGlobalActorWithGetter<Int>
func testGenericGlobalActorWithGetter() async {
}

actor RedActorImpl {
  // CHECK-LABEL: sil hidden [ossa] @$s4test12RedActorImplC5helloyySiF : $@convention(method) (Int, @sil_isolated @guaranteed RedActorImpl) -> () {
  // CHECK-NOT: hop_to_executor
  // CHECK: } // end sil function '$s4test12RedActorImplC5helloyySiF'
  func hello(_ x : Int) {}
}

actor BlueActorImpl {
// CHECK-LABEL: sil hidden [ossa] @$s4test13BlueActorImplC4poke6personyAA03RedcD0C_tYaF : $@convention(method) @async (@guaranteed RedActorImpl, @sil_isolated @guaranteed BlueActorImpl) -> () {
// CHECK:       bb0([[RED:%[0-9]+]] : @guaranteed $RedActorImpl, [[BLUE:%[0-9]+]] : @guaranteed $BlueActorImpl):
// CHECK:         hop_to_executor [[BLUE]] : $BlueActorImpl
// CHECK:         [[INTARG:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:         [[METH:%[0-9]+]] = class_method [[RED]] : $RedActorImpl, #RedActorImpl.hello : (isolated RedActorImpl) -> (Int) -> (), $@convention(method) (Int, @sil_isolated @guaranteed RedActorImpl) -> ()
// CHECK:         hop_to_executor [[RED]] : $RedActorImpl
// CHECK-NEXT:    {{%[0-9]+}} = apply [[METH]]([[INTARG]], [[RED]]) : $@convention(method) (Int, @sil_isolated @guaranteed RedActorImpl) -> ()
// CHECK-NEXT:    hop_to_executor [[BLUE]] : $BlueActorImpl
// CHECK: } // end sil function '$s4test13BlueActorImplC4poke6personyAA03RedcD0C_tYaF'
  func poke(person red : RedActorImpl) async {
    await red.hello(42)
  }

// CHECK-LABEL: sil hidden [ossa] @$s4test13BlueActorImplC14createAndGreetyyYaF : $@convention(method) @async (@sil_isolated @guaranteed BlueActorImpl) -> () {
// CHECK:     bb0([[BLUE:%[0-9]+]] : @guaranteed $BlueActorImpl):
// CHECK:       hop_to_executor [[BLUE]] : $BlueActorImpl
// CHECK:       [[RED:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(method) (@thick RedActorImpl.Type) -> @owned RedActorImpl
// CHECK:       [[REDMOVE:%[0-9]+]] = move_value [lexical] [var_decl] [[RED]] : $RedActorImpl
// CHECK:       [[REDBORROW:%[0-9]+]] = begin_borrow [[REDMOVE]] : $RedActorImpl
// CHECK:       [[INTARG:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       [[METH:%[0-9]+]] = class_method [[REDBORROW]] : $RedActorImpl, #RedActorImpl.hello : (isolated RedActorImpl) -> (Int) -> (), $@convention(method) (Int, @sil_isolated @guaranteed RedActorImpl) -> ()
// CHECK:       hop_to_executor [[REDBORROW]] : $RedActorImpl
// CHECK-NEXT:  = apply [[METH]]([[INTARG]], [[REDBORROW]]) : $@convention(method) (Int, @sil_isolated @guaranteed RedActorImpl) -> ()
// CHECK-NEXT:  end_borrow [[REDBORROW]] : $RedActorImpl
// CHECK-NEXT:  hop_to_executor [[BLUE]] : $BlueActorImpl
// CHECK:       destroy_value [[REDMOVE]] : $RedActorImpl
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
// CHECK-NEXT:  end_borrow [[REDEXE]]
      // ---- now invoke redFn, hop back to Blue, and clean-up ----
// CHECK-NEXT:  {{%[0-9]+}} = apply [[CALLEE]]([[ARG]]) : $@convention(thin) (Int) -> ()
// CHECK-NEXT:  destroy_value [[R]] : $RedActorImpl
// CHECK-NEXT:  hop_to_executor [[BLUEEXE]] : $BlueActorImpl
// CHECK:       end_borrow [[BLUEEXE]] : $BlueActorImpl
// CHECK:       destroy_value [[B]] : $BlueActorImpl
// CHECK: } // end sil function '$s4test6blueFnyyYaF'
@BlueActor func blueFn() async {
  await redFn(100)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test20unspecifiedAsyncFuncyyYaF : $@convention(thin) @async () -> () {
// CHECK:         [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// CHECK-NEXT:    hop_to_executor [[GENERIC_EXEC]] :
// CHECK:         [[BORROW:%[0-9]+]] = begin_borrow {{%[0-9]+}} : $RedActorImpl
// CHECK-NEXT:    hop_to_executor [[BORROW]] : $RedActorImpl
// CHECK-NEXT:    end_borrow [[BORROW]] : $RedActorImpl
// CHECK-NEXT:    {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(thin) (Int) -> ()
// CHECK-NEXT:    destroy_value
// CHECK-NEXT:    hop_to_executor [[GENERIC_EXEC]]
// CHECK: } // end sil function '$s4test20unspecifiedAsyncFuncyyYaF'
func unspecifiedAsyncFunc() async {
  await redFn(200)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test27anotherUnspecifiedAsyncFuncyyAA12RedActorImplCYaF : $@convention(thin) @async (@guaranteed RedActorImpl) -> () {
// CHECK:       bb0([[RED:%[0-9]+]] : @guaranteed $RedActorImpl):
// CHECK:         [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// CHECK-NEXT:    hop_to_executor [[GENERIC_EXEC]] :
// CHECK:         [[INTARG:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:         [[METH:%[0-9]+]] = class_method [[RED]] : $RedActorImpl, #RedActorImpl.hello : (isolated RedActorImpl) -> (Int) -> (), $@convention(method) (Int, @sil_isolated @guaranteed RedActorImpl) -> ()
// CHECK-NEXT:    hop_to_executor [[RED]] : $RedActorImpl
// CHECK-NEXT:    {{%[0-9]+}} = apply [[METH]]([[INTARG]], [[RED]]) : $@convention(method) (Int, @sil_isolated @guaranteed RedActorImpl) -> ()
// CHECK-NEXT:    hop_to_executor [[GENERIC_EXEC]]
// CHECK: } // end sil function '$s4test27anotherUnspecifiedAsyncFuncyyAA12RedActorImplCYaF'
func anotherUnspecifiedAsyncFunc(_ red : RedActorImpl) async {
  await red.hello(12);
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A20GlobalActorFuncValueyyyyAA03RedC0VYcXEYaF
// CHECK: [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// CHECK: hop_to_executor [[GENERIC_EXEC]] :
// CHECK: function_ref @$s4test8RedActorV6sharedAA0bC4ImplCvgZ
// CHECK: hop_to_executor [[RED:%[0-9]+]] : $RedActorImpl
// CHECK-NEXT: end_borrow [[RED]]
// CHECK-NEXT: begin_borrow
// CHECK-NEXT: apply
// CHECK:      hop_to_executor [[GENERIC_EXEC:%[0-9]+]] : $Optional<Builtin.Executor>
func testGlobalActorFuncValue(_ fn: @RedActor () -> Void) async {
  await fn()
}

func acceptAsyncSendableClosureInheriting<T>(@_inheritActorContext _: @Sendable () async -> T) { }
func acceptAsyncSendableClosureAlwaysInheriting<T>(@_inheritActorContext(always) _: @Sendable () async -> T) { }
func acceptAsyncSendableClosureAlwaysInheritingErased<T>(@_inheritActorContext(always) _: sending @isolated(any) () async -> T) { }

extension MyActor {
  func synchronous() { }

  // CHECK-LABEL: sil private [ossa] @$s4test7MyActorC0A10InheritingyyFyyYaYbXEfU_
  // CHECK: debug_value [[SELF:%[0-9]+]] : $MyActor
  // CHECK-NEXT: [[COPY:%[0-9]+]] = copy_value [[SELF]] : $MyActor
  // CHECK-NEXT: [[BORROW:%[0-9]+]] = begin_borrow [[COPY]] : $MyActor
  // CHECK-NEXT: hop_to_executor [[BORROW]] : $MyActor
  func testInheriting() {
    acceptAsyncSendableClosureInheriting {
      synchronous()
    }
  }

  // CHECK-LABEL: sil private [ossa] @$s4test7MyActorC0A16AlwaysInheritingyyFyyYaYbXEfU_
  // CHECK: debug_value [[SELF:%[0-9]+]] : $MyActor
  // CHECK-NEXT: [[COPY:%[0-9]+]] = copy_value [[SELF]] : $MyActor
  // CHECK-NEXT: [[BORROW:%[0-9]+]] = begin_borrow [[COPY]] : $MyActor
  // CHECK-NEXT: hop_to_executor [[BORROW]] : $MyActor
  func testAlwaysInheriting() {
    acceptAsyncSendableClosureAlwaysInheriting {
    }
  }
}

func testIsolatedParameterWithInheritActorContext(_ isolation: isolated (any Actor)?) {
  // CHECK-LABEL: sil private [ossa] @$s4test0A40IsolatedParameterWithInheritActorContextyyScA_pSgYiFyyYaYbXEfU_
  // CHECK: debug_value [[ISOLATION:%[0-9]+]] : $Optional<any Actor>
  // CHECK-NEXT: [[COPY:%[0-9]+]] = copy_value [[ISOLATION]] : $Optional<any Actor>
  // CHECK-NEXT: [[BORROW:%[0-9]+]] = begin_borrow [[COPY]] : $Optional<any Actor>
  // CHECK-NEXT: hop_to_executor [[BORROW]] : $Optional<any Actor>
  acceptAsyncSendableClosureAlwaysInheriting {
  }

  // CHECK-LABEL: sil private [ossa] @$s4test0A40IsolatedParameterWithInheritActorContextyyScA_pSgYiFyyYaYbScMYcXEfU0_
  // CHECK: hop_to_executor {{.*}} : $MainActor
  acceptAsyncSendableClosureAlwaysInheriting { @MainActor in
    // CHECK-LABEL: sil private [ossa] @$s4test0A40IsolatedParameterWithInheritActorContextyyScA_pSgYiFyyYaYbScMYcXEfU0_yyYaYbXEfU_
    // CHECK: hop_to_executor {{.*}} : $MainActor
    acceptAsyncSendableClosureAlwaysInheriting {
    }
  }

  // CHECK-LABEL: sil private [ossa] @$s4test0A40IsolatedParameterWithInheritActorContextyyScA_pSgYiFyyYaYbXEfU1_
  // CHECK: debug_value [[ISOLATION:%[0-9]+]] : $Optional<any Actor>
  // CHECK-NEXT: [[COPY:%[0-9]+]] = copy_value [[ISOLATION]] : $Optional<any Actor>
  // CHECK-NEXT: [[BORROW:%[0-9]+]] = begin_borrow [[COPY]] : $Optional<any Actor>
  // CHECK-NEXT: hop_to_executor [[BORROW]] : $Optional<any Actor>
  acceptAsyncSendableClosureAlwaysInheriting {
    // CHECK-LABEL: sil private [ossa] @$s4test0A40IsolatedParameterWithInheritActorContextyyScA_pSgYiFyyYaYbXEfU1_yyYaYbXEfU_
    // CHECK: debug_value [[ISOLATION:%[0-9]+]] : $Optional<any Actor>
    // CHECK-NEXT: [[COPY:%[0-9]+]] = copy_value [[ISOLATION]] : $Optional<any Actor>
    // CHECK-NEXT: [[BORROW:%[0-9]+]] = begin_borrow [[COPY]] : $Optional<any Actor>
    // CHECK-NEXT: hop_to_executor [[BORROW]] : $Optional<any Actor>
    acceptAsyncSendableClosureAlwaysInheriting {
    }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0a17IsolatedParamWithB3AnyyyScA_pYiF
// CHECK: [[CLOSURE_REF:%.*]] = function_ref @$s4test0a17IsolatedParamWithB3AnyyyScA_pYiFyyYaXEfU_
// CHECK-NEXT: [[ISOLATION_COPY_1:%.*]] = copy_value %0 : $any Actor
// CHECK-NEXT: [[ISOLATION_COPY_2:%.*]] = copy_value [[ISOLATION_COPY_1]] : $any Actor
// CHECK-NEXT: [[DYNAMIC_ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ISOLATION_COPY_2]] : $any Actor
// CHECK-NEXT: [[CLOSURE_WITH_APPLIED_ISOLATION:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_REF]]([[DYNAMIC_ISOLATION]], [[ISOLATION_COPY_1]])
func testIsolatedParamWithIsolatedAny(_ isolation: isolated any Actor) {
  // CHECK-LABEL: sil private [ossa] @$s4test0a17IsolatedParamWithB3AnyyyScA_pYiFyyYaXEfU_
  // CHECK: bb0(%0 : $*(), [[DYNAMIC_ISOLATION:%[0-9]+]] : @guaranteed $Optional<any Actor>, [[CAPTURED_ISOLATION:%[0-9]+]] : @closureCapture @guaranteed $any Actor):
  // CHECK: [[COPY:%[0-9]+]] = copy_value [[CAPTURED_ISOLATION]] : $any Actor
  // CHECK-NEXT: [[BORROW:%[0-9]+]] = begin_borrow [[COPY]] : $any Actor
  // CHECK-NEXT: hop_to_executor [[BORROW]] : $any Actor
  acceptAsyncSendableClosureAlwaysInheritingErased {
  }
}

func acceptAsyncClosure(_: () async -> Void) { }
func acceptAsyncClosure2<T>(_: (T) async -> T) { }

public actor MyPublicActor {}

@globalActor
public struct PublicGlobalActor {
  public static var shared: MyPublicActor = MyPublicActor()
}

@globalActor
private struct PrivateGlobalActor {
  static var shared: MyActor = MyActor()
}

func testGlobalActorClosure() {
  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] [ossa] @$sIeg_IegH_TR4test11GlobalActorVTU
  // CHECK:         hop_to_executor {{%.*}} : $MyActor
  // CHECK:         apply %0()
  let ga: @GlobalActor () -> () = { @GlobalActor in print(5) }
  acceptAsyncClosure(ga)

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] [ossa] @$sIeg_IegH_TR4test8RedActorVTU
  // CHECK:         hop_to_executor {{%.*}} : $RedActor
  // CHECK:         apply %0()
  let ra: @RedActor () -> () = { @RedActor in print(5) }
  acceptAsyncClosure(ra)

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] [ossa] @$sIeg_IegH_TR4test9BlueActorVTU
  // CHECK:         hop_to_executor {{%.*}} : $BlueActor
  // CHECK:         apply %0()
  let ba: @BlueActor () -> () = { @BlueActor in print(5) }
  acceptAsyncClosure(ba)

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] [ossa] @$sIeg_IegH_TR4test18PrivateGlobalActor{{.*}}VTU
  // CHECK:         hop_to_executor {{%.*}} : $MyActor
  // CHECK:         apply %0()
  let pga: @PrivateGlobalActor () -> () = { @PrivateGlobalActor in print(5) }
  acceptAsyncClosure(pga)

  // CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIeg_IegH_TR4test17PublicGlobalActorVTU
  // CHECK:         hop_to_executor {{%.*}} : $MyPublicActor
  // CHECK:         apply %0()
  let pbga: @PublicGlobalActor () -> () = { @PublicGlobalActor in print(5) }
  acceptAsyncClosure(pbga)

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] [ossa] @$sIeg_IegH_TR4test28GenericGlobalActorWithGetterVySiGTU
  // CHECK:         hop_to_executor {{%.*}} : $MyGenericActor<Int>
  // CHECK:         apply %0()
  let ggai: @GenericGlobalActorWithGetter<Int> () -> ()
    = { @GenericGlobalActorWithGetter<Int> in print(5) }
  acceptAsyncClosure(ggai)

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] [ossa] @$sIeg_IegH_TR4test28GenericGlobalActorWithGetterVySSGTU
  // CHECK:         hop_to_executor {{%.*}} : $MyGenericActor<String>
  // CHECK:         apply %0()
  let ggas: @GenericGlobalActorWithGetter<String> () -> ()
    = { @GenericGlobalActorWithGetter<String> in print(5) }
  acceptAsyncClosure(ggas)
}

func testGenericGlobalActorClosure<T>(_: T) {
  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] [ossa] @$sxxIegnr_xxIegHnr_lTR4test28GenericGlobalActorWithGetterVyxGTU 
  // CHECK:         hop_to_executor {{%.*}} : $MyGenericActor<T>
  // CHECK:         apply %2(%0, %1)
  let ggat: @GenericGlobalActorWithGetter<T> (T) -> T
    = { @GenericGlobalActorWithGetter<T> x in x }
  acceptAsyncClosure2(ggat)
}

func acceptIsolatedParam(_: Int, _: isolated MyActor, _: Double) { }

extension MyActor {
  nonisolated func otherIsolated(_: Int, _: isolated MyActor, _: Double) { }
}

// CHECK: sil hidden [ossa] @$s4test0A26ImplicitAsyncIsolatedParam1i1d5actor10otherActorySi_SdAA02MyH0CAHtYaF
func testImplicitAsyncIsolatedParam(
  i: Int, d: Double, actor: MyActor, otherActor: MyActor
) async {
  // CHECK: [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
  // CHECK-NEXT: hop_to_executor [[GENERIC_EXEC]] :
  // CHECK: [[FN1:%.*]] = function_ref @$s4test19acceptIsolatedParamyySi_AA7MyActorCYiSdtF
  // CHECK-NEXT: hop_to_executor [[ACTOR:%.*]] : $MyActor
  // CHECK-NEXT: apply [[FN1]](%0, %2, %1) : $@convention(thin) (Int, @sil_isolated @guaranteed MyActor, Double) -> ()
  // CHECK-NEXT: hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
  await acceptIsolatedParam(i, actor, d)

  // CHECK: [[FN2:%.*]] = function_ref @$s4test7MyActorC13otherIsolatedyySi_ACYiSdtF : $@convention(method) (Int, @sil_isolated @guaranteed MyActor, Double, @guaranteed MyActor) -> ()
  // CHECK-NEXT: hop_to_executor [[ACTOR:%.*]] : $MyActor
  // CHECK-NEXT: apply [[FN2]](%0, %2, %1, %3) : $@convention(method) (Int, @sil_isolated @guaranteed MyActor, Double, @guaranteed MyActor) -> ()
  // CHECK-NEXT: hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
  await otherActor.otherIsolated(i, actor, d)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test22asyncWithIsolatedParam1i5actor1dySi_AA7MyActorCYiSdtYaF : $@convention(thin) @async (Int, @sil_isolated @guaranteed MyActor, Double) -> ()
func asyncWithIsolatedParam(i: Int, actor: isolated MyActor, d: Double) async {
  // CHECK: [[ACTOR:%.*]] = copy_value %1 : $MyActor
  // CHECK-NEXT: [[BORROWED:%.*]] = begin_borrow [[ACTOR]] : $MyActor
  // CHECK-NEXT: hop_to_executor [[BORROWED]] : $MyActor
  // CHECK-NEXT: end_borrow [[BORROWED]] : $MyActor
}

// CHECK-LABEL: sil hidden [ossa] @$s4test26asyncWithUnsafeInheritanceyyYaF
@_unsafeInheritExecutor
func asyncWithUnsafeInheritance() async {
  // CHECK-NEXT: bb0:
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[FN:%.*]] = function_ref
  // CHECK-NEXT:   apply [[FN]]()
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
  await unspecifiedAsync()
}

// CHECK-LABEL: sil hidden [ossa] @$s4test34asyncWithUnsafeInheritance_hopbackyyYaF
@_unsafeInheritExecutor
func asyncWithUnsafeInheritance_hopback() async {
  // CHECK:      [[FN:%.*]] = function_ref @$s4test5redFnyySiF
  // CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thin RedActor.Type
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[ACTOR_GETTER:%.*]] = function_ref @$s4test8RedActorV6sharedAA0bC4ImplCvgZ
  // CHECK-NEXT: [[ACTOR:%.*]] = apply [[ACTOR_GETTER]]([[METATYPE]])
  // CHECK-NEXT: [[BORROWED_ACTOR:%.*]] = begin_borrow [[ACTOR]]
  // CHECK-NEXT: hop_to_executor [[BORROWED_ACTOR]]
  // CHECK-NEXT: end_borrow [[BORROWED_ACTOR]]
  // CHECK-NEXT: apply [[FN]]({{%.*}})
  // CHECK-NEXT: destroy_value [[ACTOR]]
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
  await redFn(0)
}

// Previously we would break Ownership SSA when passing a below since when we
// emitted the hop_to_executor, we would unnecessarily borrow a over the entire
// apply (we only needed it to call hop_to_executor). This would cause an OSSA
// violation since we are going to consume it as part of calling Klass's
// initializer.

// CHECK-LABEL: sil hidden [ossa] @$s4test40validateHopToExecutorLifetimeShortEnoughyyYaF : $@convention(thin) @async () -> () {
// CHECK: hop_to_executor %0
// CHECK: [[ACTOR:%.*]] = init_existential_ref {{%.*}}
// CHECK: [[INIT_FN:%.*]] = function_ref @$s4test40validateHopToExecutorLifetimeShortEnoughyyYaF5KlassL_C2onADScA_pYi_tcfC : $@convention(method) (@sil_isolated @owned any Actor, @thick Klass.Type) -> @owned Klass
// CHECK: [[BORROW:%.*]] = begin_borrow [[ACTOR]]
// CHECK: hop_to_executor [[BORROW]]
// CHECK: end_borrow [[BORROW]]
// CHECK: apply [[INIT_FN]]([[ACTOR]],
// CHECK: hop_to_executor %0
// CHECK: } // end sil function '$s4test40validateHopToExecutorLifetimeShortEnoughyyYaF'
func validateHopToExecutorLifetimeShortEnough() async {
  class Klass {
    init(
      on isolation: isolated Actor,
    ) { }
  }

  let a = MyActor()
  _ = await Klass(on: a)
}
