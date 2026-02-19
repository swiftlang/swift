// RUN: %target-swift-frontend -swift-version 6 -target %target-swift-5.1-abi-triple %s -emit-silgen -o - | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 6 -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify

// README: This file contains FileCheck tests that validate that specific Swift
// entities have their respective SILFunctions assigned the correct actor
// isolation by FileChecking against SILGen.

////////////////////////
// MARK: Declarations //
////////////////////////

func useValueAsync<T>(_ t: T) async {}

actor CustomActorInstance {}

@globalActor
struct CustomActor {
  static let shared = CustomActorInstance()
}

class NonSendableKlass {}

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

@MainActor
struct MainActorStruct {
  // CHECK: variable initialization expression of MainActorStruct.mainActorField
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  // CHECK: MainActorStruct.mainActorField.getter
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  // CHECK: MainActorStruct.mainActorField.setter
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  var mainActorField = NonSendableKlass()

  // CHECK: variable initialization expression of MainActorStruct.customActorField
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  // CHECK: MainActorStruct.customActorField.getter
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  // CHECK: MainActorStruct.customActorField.setter
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  @CustomActor var customActorField = NonSendableKlass()

  nonisolated init() {}
}

struct NonisolatedStruct {
  // CHECK: variable initialization expression of NonisolatedStruct.nonisolatedField
  // CHECK-NEXT: Isolation: unspecified
  // CHECK: NonisolatedStruct.nonisolatedField.getter
  // CHECK-NEXT: Isolation: unspecified
  // CHECK: NonisolatedStruct.nonisolatedField.setter
  // CHECK-NEXT: Isolation: unspecified
  var nonisolatedField = NonSendableKlass()

  // CHECK: variable initialization expression of NonisolatedStruct.mainActorField
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  // CHECK: NonisolatedStruct.mainActorField.getter
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  // CHECK: NonisolatedStruct.mainActorField.setter
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  @MainActor var mainActorField = NonSendableKlass()

  // CHECK: variable initialization expression of NonisolatedStruct.customActorField
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  // CHECK: NonisolatedStruct.customActorField.getter
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  // CHECK: NonisolatedStruct.customActorField.setter
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  @CustomActor var customActorField = NonSendableKlass()

  nonisolated init() {}
}

@MainActor
struct MainActorKlass {
  // CHECK: variable initialization expression of MainActorKlass.mainActorField
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  // CHECK: MainActorKlass.mainActorField.getter
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  // CHECK: MainActorKlass.mainActorField.setter
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  var mainActorField = NonSendableKlass()

  // CHECK: variable initialization expression of MainActorKlass.customActorField
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  // CHECK: MainActorKlass.customActorField.getter
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  // CHECK: MainActorKlass.customActorField.setter
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  @CustomActor var customActorField = NonSendableKlass()

  nonisolated init() {}
}

struct NonisolatedKlass {
  // CHECK: variable initialization expression of NonisolatedKlass.nonisolatedField
  // CHECK-NEXT: Isolation: unspecified
  // CHECK: NonisolatedKlass.nonisolatedField.getter
  // CHECK-NEXT: Isolation: unspecified
  // CHECK: NonisolatedKlass.nonisolatedField.setter
  // CHECK-NEXT: Isolation: unspecified
  var nonisolatedField = NonSendableKlass()

  // CHECK: variable initialization expression of NonisolatedKlass.mainActorField
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  // CHECK: NonisolatedKlass.mainActorField.getter
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  // CHECK: NonisolatedKlass.mainActorField.setter
  // CHECK-NEXT: Isolation: global_actor. type: MainActor
  @MainActor var mainActorField = NonSendableKlass()

  // CHECK: variable initialization expression of NonisolatedKlass.customActorField
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  // CHECK: NonisolatedKlass.customActorField.getter
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  // CHECK: NonisolatedKlass.customActorField.setter
  // CHECK-NEXT: Isolation: global_actor. type: CustomActor
  @CustomActor var customActorField = NonSendableKlass()

  nonisolated init() {}
}
