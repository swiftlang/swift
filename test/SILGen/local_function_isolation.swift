// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -target %target-swift-5.1-abi-triple | %FileCheck %s

// REQUIRES: concurrency

class NotSendable {}

func callee(_ ns: NotSendable) {}

actor MyActor {
  func isolatedToSelf(ns: NotSendable) {
    // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation7MyActorC14isolatedToSelf2nsyAA11NotSendableC_tF08implicitH7CaptureL_yyYaF : $@convention(thin) @async (@guaranteed NotSendable, @sil_isolated @guaranteed MyActor) -> () {
    func implicitSelfCapture() async {

      // CHECK: [[COPY:%.*]] = copy_value %1 : $MyActor
      // CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [[COPY]] : $MyActor
      // CHECK-NEXT: hop_to_executor [[BORROW]] : $MyActor

      // CHECK: [[FN:%.*]] = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
      // CHECK-NEXT: apply [[FN]]() : $@convention(thin) @async () -> ()
      await test()

      // CHECK: hop_to_executor [[BORROW]] : $MyActor
      // CHECK: [[FN:%.*]] = function_ref @$s24local_function_isolation6calleeyyAA11NotSendableCF : $@convention(thin) (@guaranteed NotSendable) -> ()
      // CHECK-NEXT: apply [[FN]](%0) : $@convention(thin) (@guaranteed NotSendable) -> ()

      // we need to hop back to 'self' here
      callee(ns)

      // CHECK: end_borrow [[BORROW]] : $MyActor
      // CHECK-NEXT: destroy_value [[COPY]] : $MyActor
    }
  }
}

func f(isolation: isolated MyActor, ns: NotSendable) {
  // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation1f0C02nsyAA7MyActorCYi_AA11NotSendableCtF23implicitIsolatedCaptureL_yyYaF : $@convention(thin) @async (@guaranteed NotSendable, @sil_isolated @guaranteed MyActor) -> () {
  func implicitIsolatedCapture() async {

    // CHECK: [[COPY:%.*]] = copy_value %1 : $MyActor
    // CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [[COPY]] : $MyActor
    // CHECK-NEXT: hop_to_executor [[BORROW]] : $MyActor

    // CHECK: [[FN:%.*]] = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
    // CHECK-NEXT: apply [[FN]]() : $@convention(thin) @async () -> ()
    await test()

    // we need to hop back to 'isolation' here
    callee(ns)

    // CHECK: end_borrow [[BORROW]] : $MyActor
    // CHECK-NEXT: destroy_value [[COPY]] : $MyActor
  }
}

func test() async {}

// A generic actor type, which causes the generic self parameter
// (actor isolation parameter) to be added to captures of the
// local/nested function.
actor GenericActor<K> {
  var i: Int = 0
  func outerFunc() async {
    func accessSelf() async -> Int {
      // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation12GenericActorC9outerFuncyyYaF10accessSelfL_SiyYalF : $@convention(thin) @async <K> (@sil_isolated @guaranteed GenericActor<K>) -> Int {
      return 0
    }
    await print(accessSelf())
  }
}

// Make sure defer doesn't capture anything.
actor DeferInsideInitActor {
  init(foo: ()) async throws {
    // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation20DeferInsideInitActorC3fooACyt_tYaKcfc6$deferL_yyF : $@convention(thin) () -> () {
    defer {}
    self.init()
  }
}

actor NestedAsyncInSyncActor {
  public func outer() async {
    // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation22NestedAsyncInSyncActorC5outeryyYaF6middleL_yyF : $@convention(thin) (@sil_isolated @guaranteed NestedAsyncInSyncActor) -> () {
    func middle() {
      // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation22NestedAsyncInSyncActorC5outeryyYaF6middleL_yyF5innerL_yyYaF : $@convention(thin) @async (@sil_isolated @guaranteed NestedAsyncInSyncActor) -> () {
      func inner() async {}
      _ = inner
    }
    _ = middle
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s24local_function_isolation13outerFunctionyyScA_pYaF : $@convention(thin) @async (@guaranteed any Actor) -> () {
func outerFunction(_ a: any Actor) async {
  // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation13outerFunctionyyScA_pYaF06middleE0L_yyScA_pYaF : $@convention(thin) @async (@guaranteed any Actor) -> () {
  func middleFunction(_ isolated: any Actor) async {
    // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation13outerFunctionyyScA_pYaF06middleE0L_yyScA_pYaF05innerE0L_yyYaF : $@convention(thin) @async () -> () {
    func innerFunction() async {}
  }

  await middleFunction(a)
}

// Check async defer matches enclosing isolation
actor AsyncDeferActor {
  func foo() async {
    // CHECK-LABEL: // $defer #1 () in AsyncDeferActor.foo()
    // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation15AsyncDeferActorC3fooyyYaF6$deferL_yyYaF : $@convention(thin) @async (@sil_isolated @guaranteed AsyncDeferActor) -> () {
    // CHECK-NEXT: // %0 "self" // users: %2, %1
    // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $AsyncDeferActor):
    defer {
      await test()
    }

    await test()
  }
}


func testAsyncDeferIsolatedParam(isolated: isolated (any Actor)? = nil) async {
  let x: Int = 0

  // CHECK-LABEL: // $defer #1 () in testAsyncDeferIsolatedParam(isolated:)
  // CHECK-NEXT: // Isolation: actor_instance. name: 'isolated'
  // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation27testAsyncDeferIsolatedParam8isolatedyScA_pSgYi_tYaF6$deferL_yyYaF : $@convention(thin) @async (Int, @sil_isolated @guaranteed Optional<any Actor>) -> () {
  // CHECK-NEXT: // %0 "x" // users: %10, %2
  // CHECK-NEXT: // %1 "isolated" // users: %4, %3
  // CHECK-NEXT: bb0(%0 : @closureCapture $Int, %1 : @closureCapture @guaranteed $Optional<any Actor>):
  defer {
    await test()
    _ = x
  }

  _ = x


  let _: @MainActor () async -> Void = {
    let z: Int = 0

    // CHECK-LABEL: // $defer #1 () in closure #1 in testAsyncDeferIsolatedParam(isolated:)
    // CHECK-NEXT: // Isolation: global_actor. type: MainActor
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation27testAsyncDeferIsolatedParam8isolatedyScA_pSgYi_tYaFyyYaScMYccfU_6$deferL_yyYaF : $@convention(thin) @async (Int) -> () {
    // CHECK-NEXT: // %0 "z" // users: %7, %1
    // CHECK-NEXT: bb0(%0 : @closureCapture $Int):
    defer {
      await test()
      _ = z
    }

    _ = z

    func inner(isolated: isolated (any Actor)? = nil) async {
      let y: Int = 0
      // CHECK-LABEL: // $defer #1 () in inner #1 (isolated:) in closure #1 in testAsyncDeferIsolatedParam(isolated:)
      // CHECK-NEXT: // Isolation: actor_instance. name: 'isolated'
      // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation27testAsyncDeferIsolatedParam8isolatedyScA_pSgYi_tYaFyyYaScMYccfU_5innerL_ACyADYi_tYaF6$deferL_yyYaF : $@convention(thin) @async (@sil_isolated @guaranteed Optional<any Actor>, Int) -> () {
      // CHECK-NEXT: // %0 "isolated" // users: %12, %4, %2
      // CHECK-NEXT: // %1 "y" // users: %10, %3
      // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $Optional<any Actor>, %1 : @closureCapture $Int): 
      defer {
        await test()

        // CHECK-LABEL: // $defer #1 () in $defer #1 () in inner #1 (isolated:) in closure #1 in testAsyncDeferIsolatedParam(isolated:)
        // CHECK-NEXT: // Isolation: actor_instance. name: 'isolated'
        // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation27testAsyncDeferIsolatedParam8isolatedyScA_pSgYi_tYaFyyYaScMYccfU_5innerL_ACyADYi_tYaF6$deferL_yyYaFAFL_yyYaF : $@convention(thin) @async (@sil_isolated @guaranteed Optional<any Actor>) -> () {
        // CHECK-NEXT: // %0 "isolated" // users: %2, %1
        // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $Optional<any Actor>):
        defer {
          await test()
        }
        _ = y
      }

      _ = y
    }
  }
}
