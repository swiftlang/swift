// RUN: %target-swift-frontend -swift-version 6 -target %target-swift-5.1-abi-triple %s -emit-silgen -o - | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 6 -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify

// README: This file contains FileCheck tests that validate that specific Swift
// entities have their respective SILFunctions assigned the correct actor
// isolation by FileChecking against SILGen.

////////////////////////
// MARK: Declarations //
////////////////////////

func useValueAsync<T>(_ t: T) async {}

/////////////////
// MARK: Tests //
/////////////////

// CHECK: // synchronousActorIsolatedFinalClassMethodError()
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-NEXT: sil hidden [ossa] @$s25actor_isolation_filecheck45synchronousActorIsolatedFinalClassMethodErroryyYaF : $@convention(thin) @async () -> () {
@MainActor func synchronousActorIsolatedFinalClassMethodError() async {
  @MainActor final class Test {
    // CHECK: // foo() in Test #1 in synchronousActorIsolatedFinalClassMethodError()
    // CHECK-NEXT: // Isolation: global_actor. type: MainActor
    // CHECK-NEXT: sil private [ossa] @$s25actor_isolation_filecheck45synchronousActorIsolatedFinalClassMethodErroryyYaF4TestL_C3fooyyF : $@convention(method) (@guaranteed Test) -> () {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-error {{sending 'erased' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'erased' to nonisolated global function 'useValueAsync' risks causing data races between nonisolated and main actor-isolated uses}}
}
