// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency | %FileCheck %s
// REQUIRES: concurrency


actor class MyActor {

  private var p: Int

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC6calleeyySiYF : $@convention(method) @async (Int, @guaranteed MyActor) -> () {
  // CHECK-NOT:   hop_to_executor
  // CHECK:     } // end sil function '$s4test7MyActorC6calleeyySiYF'
  @actorIndependent
  func callee(_ x: Int) async {
    print(x)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC14throwingCalleeyySiYKF : $@convention(method) @async (Int, @guaranteed MyActor) -> @error Error {
  // CHECK-NOT:   hop_to_executor
  // CHECK:     } // end sil function '$s4test7MyActorC14throwingCalleeyySiYKF'
  @actorIndependent
  func throwingCallee(_ x: Int) async throws {
    print(x)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC0A13AsyncFunctionyyYKF : $@convention(method) @async (@guaranteed MyActor) -> @error Error {
  // CHECK:        hop_to_executor %0 : $MyActor 
  // CHECK:        = apply {{.*}} : $@convention(method) @async (Int, @guaranteed MyActor) -> ()
  // CHECK-NEXT:   hop_to_executor %0 : $MyActor 
  // CHECK:        try_apply {{.*}}, normal bb1, error bb2
  // CHECK:      bb1({{.*}}):
  // CHECK-NEXT:   hop_to_executor %0 : $MyActor 
  // CHECK:      bb2({{.*}}):
  // CHECK-NEXT:   hop_to_executor %0 : $MyActor 
  // CHECK:      } // end sil function '$s4test7MyActorC0A13AsyncFunctionyyYKF'
  func testAsyncFunction() async throws {
    await callee(p)
    await try throwingCallee(p)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC0A22ConsumingAsyncFunctionyyYF : $@convention(method) @async (@owned MyActor) -> () {
  // CHECK:        [[BORROWED_SELF:%[0-9]+]] = begin_borrow %0 : $MyActor
  // CHECK:        hop_to_executor [[BORROWED_SELF]] : $MyActor 
  // CHECK:        = apply {{.*}} : $@convention(method) @async (Int, @guaranteed MyActor) -> ()
  // CHECK-NEXT:   hop_to_executor [[BORROWED_SELF]] : $MyActor 
  // CHECK:      } // end sil function '$s4test7MyActorC0A22ConsumingAsyncFunctionyyYF'
  __consuming func testConsumingAsyncFunction() async {
    await callee(p)
  }


  // CHECK-LABEL: sil private [ossa] @$s4test7MyActorC0A7ClosureSiyYFSiyYXEfU_ : $@convention(thin) @async (@guaranteed MyActor) -> Int {
  // CHECK:        [[COPIED_SELF:%[0-9]+]] = copy_value %0 : $MyActor
  // CHECK:        [[BORROWED_SELF:%[0-9]+]] = begin_borrow [[COPIED_SELF]] : $MyActor
  // CHECK:        hop_to_executor [[BORROWED_SELF]] : $MyActor 
  // CHECK:        = apply
  // CHECK:      } // end sil function '$s4test7MyActorC0A7ClosureSiyYFSiyYXEfU_'
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

// CHECK-LABEL: sil hidden [ossa] @$s4test0A11GlobalActoryyYF : $@convention(thin) @async () -> () {
// CHECK:   [[F:%[0-9]+]] = function_ref @$s4test11GlobalActorV6sharedAA02MyC0Cvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[P:%[0-9]+]] = apply [[F]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[A:%[0-9]+]] = pointer_to_address %2 : $Builtin.RawPointer to [strict] $*MyActor
// CHECK:   [[ACC:%[0-9]+]] = begin_access [read] [dynamic] [[A]] : $*MyActor
// CHECK:   [[L:%[0-9]+]] = load [copy] [[ACC]] : $*MyActor
// CHECK:   [[B:%[0-9]+]] = begin_borrow [[L]] : $MyActor
// CHECK:   hop_to_executor [[B]] : $MyActor
// CHECK: } // end sil function '$s4test0A11GlobalActoryyYF'
@GlobalActor
func testGlobalActor() async {
}

// CHECK-LABEL: sil private [ossa] @$s4test0A22GlobalActorWithClosureyyYFyyYXEfU_ : $@convention(thin) @async () -> () {
// CHECK:   [[F:%[0-9]+]] = function_ref @$s4test11GlobalActorV6sharedAA02MyC0Cvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[P:%[0-9]+]] = apply [[F]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[A:%[0-9]+]] = pointer_to_address %2 : $Builtin.RawPointer to [strict] $*MyActor
// CHECK:   [[ACC:%[0-9]+]] = begin_access [read] [dynamic] [[A]] : $*MyActor
// CHECK:   [[L:%[0-9]+]] = load [copy] [[ACC]] : $*MyActor
// CHECK:   [[B:%[0-9]+]] = begin_borrow [[L]] : $MyActor
// CHECK:   hop_to_executor [[B]] : $MyActor
// CHECK: } // end sil function '$s4test0A22GlobalActorWithClosureyyYFyyYXEfU_'
@GlobalActor
func testGlobalActorWithClosure() async {
  await { () async in }()
}

@globalActor
struct GenericGlobalActorWithGetter<T> {
  static var shared: MyActor { return MyActor() }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A28GenericGlobalActorWithGetteryyYF : $@convention(thin) @async () -> () {
// CHECK:     [[MT:%[0-9]+]] = metatype $@thin GenericGlobalActorWithGetter<Int>.Type
// CHECK:     [[F:%[0-9]+]] = function_ref @$s4test28GenericGlobalActorWithGetterV6sharedAA02MyD0CvgZ : $@convention(method) <τ_0_0> (@thin GenericGlobalActorWithGetter<τ_0_0>.Type) -> @owned MyActor
// CHECK:     [[A:%[0-9]+]] = apply [[F]]<Int>([[MT]]) : $@convention(method) <τ_0_0> (@thin GenericGlobalActorWithGetter<τ_0_0>.Type) -> @owned MyActor
// CHECK:     [[B:%[0-9]+]] = begin_borrow [[A]] : $MyActor
// CHECK:     hop_to_executor [[B]] : $MyActor
// CHECK: } // end sil function '$s4test0A28GenericGlobalActorWithGetteryyYF'
@GenericGlobalActorWithGetter<Int>
func testGenericGlobalActorWithGetter() async {
}

