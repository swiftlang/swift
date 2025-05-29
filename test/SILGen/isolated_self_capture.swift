// RUN: %target-swift-frontend -emit-silgen %s -module-name test -target %target-swift-5.1-abi-triple -sil-verify-all -swift-version 5 | %FileCheck %s

// REQUIRES: concurrency

class NonSendableKlass {}

func useValue<T>(_ t: T) {}

@MainActor func transferToMain<T>(_ t: T) async {}

actor MyActor {
  var ns = NonSendableKlass()

  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC19passSelfIntoClosureyyYaF : $@convention(method) @async (@sil_isolated @guaranteed MyActor) -> () {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $
  // CHECK:   [[COPY:%.*]] = copy_value [[ARG]]
  // CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] {{.*}}([[COPY]]) : $@convention(thin) (@sil_isolated @guaranteed MyActor) -> ()
  // CHECK: } // end sil function '$s4test7MyActorC19passSelfIntoClosureyyYaF'
  func passSelfIntoClosure() async {
    let closure = { useValue(self.ns) }
    await transferToMain(closure) 
  }
}
