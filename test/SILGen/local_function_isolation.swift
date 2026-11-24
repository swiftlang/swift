// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -target %target-swift-5.1-abi-triple | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -target %target-swift-5.1-abi-triple | %FileCheck %s --check-prefix=CHECK-NESTED

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

// https://github.com/swiftlang/swift/issues/93432
// An async defer in a global actor isolated closure must hop to the actor on
// entry, not to the global executor
@MainActor
final class AsyncDeferInGlobalActorClosure {
  func run() {
    Task {
      MainActor.assertIsolated("Inside task")
      // CHECK-LABEL: // $defer #1 () in closure #1 in AsyncDeferInGlobalActorClosure.run()
      // CHECK-NEXT: // Isolation: global_actor. type: MainActor
      // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation30AsyncDeferInGlobalActorClosureC3runyyFyyYacfU_6$deferL_yyYaF : $@convention(thin) @async () -> () {
      // CHECK-NEXT: bb0:

      // We must not randomly hop off:
      // CHECK-NOT: %0 = enum $Optional<any Actor>, #Optional.none!enumelt

      // We know the defer must be isolated the same way as enclosing context:
      // CHECK-NEXT: %0 = metatype $@thick MainActor.Type
      // CHECK-NEXT: // function_ref static MainActor.shared.getter
      // CHECK-NEXT: %1 = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
      // CHECK-NEXT: %2 = apply %1(%0) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
      // CHECK-NEXT: %3 = begin_borrow %2 : $MainActor
      // CHECK-NEXT: hop_to_executor %3 : $MainActor
      // CHECK: } // end sil function '$s24local_function_isolation30AsyncDeferInGlobalActorClosureC3runyyFyyYacfU_6$deferL_yyYaF'
      defer {
        MainActor.assertIsolated("Inside task")
        await test()
      }
      await test()
    }
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
    // CHECK-NEXT: // %0 "self"
    // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $AsyncDeferActor):
    defer {
      await test()
    }

    await test()
  }
}


// An async defer in a closure isolated to an actor instance must capture the
// actor and hop to it on entry.
actor AsyncDeferInActorInstanceClosure {
  func run() {
    Task {
      self.assertIsolated("Inside task")
      // CHECK-LABEL: // $defer #1 () in closure #1 in AsyncDeferInActorInstanceClosure.run()
      // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
      // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation32AsyncDeferInActorInstanceClosureC3runyyFyyYacfU_6$deferL_yyYaF : $@convention(thin) @async (@sil_isolated @guaranteed AsyncDeferInActorInstanceClosure) -> () {
      // CHECK-NEXT: // %0 "self"
      // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $AsyncDeferInActorInstanceClosure):
      // CHECK-NEXT: debug_value %0 : $AsyncDeferInActorInstanceClosure, let, name "self", argno 1
      // CHECK-NEXT: %2 = copy_value %0 : $AsyncDeferInActorInstanceClosure
      // CHECK-NEXT: %3 = begin_borrow %2 : $AsyncDeferInActorInstanceClosure
      // CHECK-NEXT: hop_to_executor %3 : $AsyncDeferInActorInstanceClosure
      // CHECK-NEXT: // function_ref test()
      // CHECK-NEXT: %5 = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
      // CHECK-NEXT: %6 = apply %5() : $@convention(thin) @async () -> ()
      // CHECK-NEXT: hop_to_executor %3 : $AsyncDeferInActorInstanceClosure
      // CHECK-NEXT: end_borrow %3 : $AsyncDeferInActorInstanceClosure
      // CHECK-NEXT: destroy_value %2 : $AsyncDeferInActorInstanceClosure
      // CHECK-NEXT: end_formal_scope %0 : $AsyncDeferInActorInstanceClosure
      // CHECK-NEXT: %11 = tuple ()
      // CHECK-NEXT: return %11 : $()
      // CHECK-NEXT: } // end sil function '$s24local_function_isolation32AsyncDeferInActorInstanceClosureC3runyyFyyYacfU_6$deferL_yyYaF'
      defer {
        await test()
      }
      await test()
    }
  }
}

// The same for a closure that is isolated to a captured 'isolated' parameter.
func asyncDeferInCapturedIsolatedParamClosure(isolated: isolated any Actor) async {
  let _: () async -> Void = {
    isolated.assertIsolated("Inside closure")
    // CHECK-LABEL: // $defer #1 () in closure #1 in asyncDeferInCapturedIsolatedParamClosure(isolated:)
    // CHECK-NEXT: // Isolation: actor_instance. name: 'isolated'
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation40asyncDeferInCapturedIsolatedParamClosure8isolatedyScA_pYi_tYaFyyYacfU_6$deferL_yyYaF : $@convention(thin) @async (@sil_isolated @guaranteed any Actor) -> () {
    // CHECK-NEXT: // %0 "isolated"
    // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $any Actor):
    // CHECK-NEXT: debug_value %0 : $any Actor, let, name "isolated", argno 1
    // CHECK-NEXT: %2 = copy_value %0 : $any Actor
    // CHECK-NEXT: %3 = begin_borrow %2 : $any Actor
    // CHECK-NEXT: hop_to_executor %3 : $any Actor
    // CHECK-NEXT: // function_ref test()
    // CHECK-NEXT: %5 = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
    // CHECK-NEXT: %6 = apply %5() : $@convention(thin) @async () -> ()
    // CHECK-NEXT: hop_to_executor %3 : $any Actor
    // CHECK-NEXT: end_borrow %3 : $any Actor
    // CHECK-NEXT: destroy_value %2 : $any Actor
    // CHECK-NEXT: end_formal_scope %0 : $any Actor
    // CHECK-NEXT: %11 = tuple ()
    // CHECK-NEXT: return %11 : $()
    // CHECK-NEXT: } // end sil function '$s24local_function_isolation40asyncDeferInCapturedIsolatedParamClosure8isolatedyScA_pYi_tYaFyyYacfU_6$deferL_yyYaF'
    defer {
      await test()
    }
    await test()
  }
}

// Nested async defers in a closure isolated to a global actor.
@MainActor
final class NestedAsyncDeferInGlobalActorClosure {
  func run() {
    // CHECK-LABEL: // $defer #1 () in closure #1 in NestedAsyncDeferInGlobalActorClosure.run()
    // CHECK-NEXT: // Isolation: global_actor. type: MainActor
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation36NestedAsyncDeferInGlobalActorClosureC3runyyFyyYacfU_6$deferL_yyYaF : $@convention(thin) @async () -> () {
    // CHECK-NESTED-LABEL: // $defer #1 () in $defer #1 () in closure #1 in NestedAsyncDeferInGlobalActorClosure.run()
    // CHECK-NESTED-NEXT: // Isolation: global_actor. type: MainActor
    // CHECK-NESTED-NEXT: sil private [ossa] @$s24local_function_isolation36NestedAsyncDeferInGlobalActorClosureC3runyyFyyYacfU_6$deferL_yyYaFAEL_yyYaF : $@convention(thin) @async () -> () {
    Task {
      defer {
        defer {
          await test()
        }
        await test()
      }
      await test()
    }
  }
}

// Nested async defers in a closure isolated to an actor instance.
actor NestedAsyncDeferInActorInstanceClosure {
  func run() {
    // CHECK-LABEL: // $defer #1 () in closure #1 in NestedAsyncDeferInActorInstanceClosure.run()
    // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation38NestedAsyncDeferInActorInstanceClosureC3runyyFyyYacfU_6$deferL_yyYaF : $@convention(thin) @async (@sil_isolated @guaranteed NestedAsyncDeferInActorInstanceClosure) -> () {
    // CHECK-NEXT: // %0 "self"
    // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $NestedAsyncDeferInActorInstanceClosure):
    // CHECK-NEXT: debug_value %0 : $NestedAsyncDeferInActorInstanceClosure, let, name "self", argno 1
    // CHECK-NEXT: %2 = copy_value %0 : $NestedAsyncDeferInActorInstanceClosure
    // CHECK-NEXT: %3 = begin_borrow %2 : $NestedAsyncDeferInActorInstanceClosure
    // CHECK-NEXT: hop_to_executor %3 : $NestedAsyncDeferInActorInstanceClosure
    // CHECK-NESTED-LABEL: // $defer #1 () in $defer #1 () in closure #1 in NestedAsyncDeferInActorInstanceClosure.run()
    // CHECK-NESTED-NEXT: // Isolation: actor_instance. name: 'self'
    // CHECK-NESTED-NEXT: sil private [ossa] @$s24local_function_isolation38NestedAsyncDeferInActorInstanceClosureC3runyyFyyYacfU_6$deferL_yyYaFAEL_yyYaF : $@convention(thin) @async (@sil_isolated @guaranteed NestedAsyncDeferInActorInstanceClosure) -> () {
    // CHECK-NESTED-NEXT: // %0 "self"
    // CHECK-NESTED-NEXT: bb0(%0 : @closureCapture @guaranteed $NestedAsyncDeferInActorInstanceClosure):
    // CHECK-NESTED-NEXT: debug_value %0 : $NestedAsyncDeferInActorInstanceClosure, let, name "self", argno 1
    // CHECK-NESTED-NEXT: %2 = copy_value %0 : $NestedAsyncDeferInActorInstanceClosure
    // CHECK-NESTED-NEXT: %3 = begin_borrow %2 : $NestedAsyncDeferInActorInstanceClosure
    // CHECK-NESTED-NEXT: hop_to_executor %3 : $NestedAsyncDeferInActorInstanceClosure
    Task {
      self.assertIsolated("Inside task")
      defer {
        defer {
          await test()
        }
        await test()
      }
      await test()
    }
  }
}

// Sync defers using '#isolation' in a closure isolated to an actor instance
// capture the isolation of the closure even though they do not name the actor
actor SyncDeferIsolationInActorInstanceClosure {
  func run() {
    // CHECK-LABEL: // $defer #1 () in closure #1 in SyncDeferIsolationInActorInstanceClosure.run()
    // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation40SyncDeferIsolationInActorInstanceClosureC3runyyFyyYacfU_6$deferL_yyF : $@convention(thin) (@sil_isolated @guaranteed SyncDeferIsolationInActorInstanceClosure) -> () {
    // CHECK-NEXT: // %0 "self"
    // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $SyncDeferIsolationInActorInstanceClosure):
    Task {
      self.assertIsolated("Inside task")
      do {
        defer {
          let seen = #isolation
        }
        await test()
      }
    }
  }
}

actor NestedSyncDeferIsolationInActorInstanceClosure {
  func run() {
    // CHECK-LABEL: // $defer #1 () in closure #1 in NestedSyncDeferIsolationInActorInstanceClosure.run()
    // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation46NestedSyncDeferIsolationInActorInstanceClosureC3runyyFyyYacfU_6$deferL_yyF : $@convention(thin) (@sil_isolated @guaranteed NestedSyncDeferIsolationInActorInstanceClosure) -> () {
    // CHECK-NEXT: // %0 "self"
    // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $NestedSyncDeferIsolationInActorInstanceClosure):
    // CHECK-NESTED-LABEL: // $defer #1 () in $defer #1 () in closure #1 in NestedSyncDeferIsolationInActorInstanceClosure.run()
    // CHECK-NESTED-NEXT: // Isolation: actor_instance. name: 'self'
    // CHECK-NESTED-NEXT: sil private [ossa] @$s24local_function_isolation46NestedSyncDeferIsolationInActorInstanceClosureC3runyyFyyYacfU_6$deferL_yyFAEL_yyF : $@convention(thin) (@sil_isolated @guaranteed NestedSyncDeferIsolationInActorInstanceClosure) -> () {
    // CHECK-NESTED-NEXT: // %0 "self"
    // CHECK-NESTED-NEXT: bb0(%0 : @closureCapture @guaranteed $NestedSyncDeferIsolationInActorInstanceClosure):
    Task {
      self.assertIsolated("Inside task")
      do {
        defer {
          defer {
            let seen = #isolation
          }
        }
        await test()
      }
    }
  }
}

// A sync defer that does not use the isolation does not capture the actor
actor PlainSyncDeferInActorInstanceClosure {
  func run() {
    // CHECK-LABEL: // $defer #1 () in closure #1 in PlainSyncDeferInActorInstanceClosure.run()
    // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation36PlainSyncDeferInActorInstanceClosureC3runyyFyyYacfU_6$deferL_yyF : $@convention(thin) () -> () {
    // CHECK-NEXT: bb0:
    Task {
      self.assertIsolated("Inside task")
      defer {
        print("done")
      }
      await test()
    }
  }
}

nonisolated(nonsending) func asyncDeferInNonsendingClosure() async {
  let closure: nonisolated(nonsending) () async -> Void = {
    // CHECK-LABEL: // $defer #1 () in closure #1 in asyncDeferInNonsendingClosure()
    // CHECK-NEXT: // Isolation: nonisolated(nonsending)
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation29asyncDeferInNonsendingClosureyyYaFyyYaYCcfU_6$deferL_yyYaF : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> () {
    // CHECK: bb0(%0 : @guaranteed $Builtin.ImplicitActor):
    // CHECK-NEXT: hop_to_executor %0 : $Builtin.ImplicitActor
    // CHECK: } // end sil function '$s24local_function_isolation29asyncDeferInNonsendingClosureyyYaFyyYaYCcfU_6$deferL_yyYaF'
    defer { await test() }
    await test()
  }
  await closure()
}

func asyncDeferInIsolatedAnyClosure() async {
  let closure: @isolated(any) () async -> Void = {
    // The closure captures no isolation, so it and its defer are @concurrent
    // CHECK-LABEL: // $defer #1 () in closure #1 in asyncDeferInIsolatedAnyClosure()
    // CHECK-NEXT: // Isolation: @concurrent
    // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation30asyncDeferInIsolatedAnyClosureyyYaFyyYacfU_6$deferL_yyYaF : $@convention(thin) @async () -> () {
    // CHECK-NEXT: bb0:
    // CHECK-NEXT: %0 = enum $Optional<any Actor>, #Optional.none!enumelt
    // CHECK-NEXT: hop_to_executor %0 : $Optional<any Actor>
    // CHECK: } // end sil function '$s24local_function_isolation30asyncDeferInIsolatedAnyClosureyyYaFyyYacfU_6$deferL_yyYaF'
    defer { await test() }
    await test()
  }
  await closure()
}

func testAsyncDeferIsolatedParam(isolated: isolated (any Actor)? = nil) async {
  let x: Int = 0

  // CHECK-LABEL: // $defer #1 () in testAsyncDeferIsolatedParam(isolated:)
  // CHECK-NEXT: // Isolation: actor_instance. name: 'isolated'
  // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation27testAsyncDeferIsolatedParam8isolatedyScA_pSgYi_tYaF6$deferL_yyYaF : $@convention(thin) @async (Int, @sil_isolated @guaranteed Optional<any Actor>) -> () {
  // CHECK-NEXT: // %0 "x"
  // CHECK-NEXT: // %1 "isolated"
  // CHECK-NEXT: bb0(%0 : @closureCapture $Int, %1 : @closureCapture @guaranteed $Optional<any Actor>):
  // CHECK-NEXT: debug_value %0 : $Int, let, name "x", argno 1
  // CHECK-NEXT: debug_value %1 : $Optional<any Actor>, let, name "isolated", argno 2
  // CHECK-NEXT: %4 = copy_value %1 : $Optional<any Actor>
  // CHECK-NEXT: %5 = begin_borrow %4 : $Optional<any Actor>
  // CHECK-NEXT: hop_to_executor %5 : $Optional<any Actor>
  // CHECK-NEXT: // function_ref test()
  // CHECK-NEXT: %7 = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
  // CHECK-NEXT: %8 = apply %7() : $@convention(thin) @async () -> ()
  // CHECK-NEXT: hop_to_executor %5 : $Optional<any Actor>
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
    // CHECK-NEXT: // %0 "z"
    // CHECK-NEXT: bb0(%0 : @closureCapture $Int):
    // CHECK-NEXT: debug_value %0 : $Int, let, name "z", argno 1
    // CHECK-NEXT: %2 = metatype $@thick MainActor.Type
    // CHECK-NEXT: // function_ref static MainActor.shared.getter
    // CHECK-NEXT: %3 = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
    // CHECK-NEXT: %4 = apply %3(%2) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
    // CHECK-NEXT: %5 = begin_borrow %4 : $MainActor
    // CHECK-NEXT: hop_to_executor %5 : $MainActor
    // CHECK-NEXT: // function_ref test()
    // CHECK-NEXT: %7 = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
    // CHECK-NEXT: %8 = apply %7() : $@convention(thin) @async () -> ()
    // CHECK-NEXT: hop_to_executor %5 : $MainActor
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
      // CHECK-NEXT: // %0 "isolated"
      // CHECK-NEXT: // %1 "y"
      // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $Optional<any Actor>, %1 : @closureCapture $Int):
      // CHECK-NEXT: debug_value %0 : $Optional<any Actor>, let, name "isolated", argno 1
      // CHECK-NEXT: debug_value %1 : $Int, let, name "y", argno 2
      // CHECK-NEXT: %4 = copy_value %0 : $Optional<any Actor>
      // CHECK-NEXT: %5 = begin_borrow %4 : $Optional<any Actor>
      // CHECK-NEXT: hop_to_executor %5 : $Optional<any Actor>
      // CHECK-NEXT: // function_ref test()
      // CHECK-NEXT: %7 = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
      // CHECK-NEXT: %8 = apply %7() : $@convention(thin) @async () -> ()
      // CHECK-NEXT: hop_to_executor %5 : $Optional<any Actor>
      defer {
        await test()

        // CHECK-LABEL: // $defer #1 () in $defer #1 () in inner #1 (isolated:) in closure #1 in testAsyncDeferIsolatedParam(isolated:)
        // CHECK-NEXT: // Isolation: actor_instance. name: 'isolated'
        // CHECK-NEXT: sil private [ossa] @$s24local_function_isolation27testAsyncDeferIsolatedParam8isolatedyScA_pSgYi_tYaFyyYaScMYccfU_5innerL_ACyADYi_tYaF6$deferL_yyYaFAFL_yyYaF : $@convention(thin) @async (@sil_isolated @guaranteed Optional<any Actor>) -> () {
        // CHECK-NEXT: // %0 "isolated"
        // CHECK-NEXT: bb0(%0 : @closureCapture @guaranteed $Optional<any Actor>):
        // CHECK-NEXT: debug_value %0 : $Optional<any Actor>, let, name "isolated", argno 1
        // CHECK-NEXT: %2 = copy_value %0 : $Optional<any Actor>
        // CHECK-NEXT: %3 = begin_borrow %2 : $Optional<any Actor>
        // CHECK-NEXT: hop_to_executor %3 : $Optional<any Actor>
        // CHECK-NEXT: // function_ref test()
        // CHECK-NEXT: %5 = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
        // CHECK-NEXT: %6 = apply %5() : $@convention(thin) @async () -> ()
        // CHECK-NEXT: hop_to_executor %3 : $Optional<any Actor>
        defer {
          await test()
        }
        _ = y
      }

      _ = y
    }
  }
}
