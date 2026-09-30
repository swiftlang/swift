// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift

// 'func x()' and 'func x() oneway' are distinct overloads, also for distributed
// funcs: if the 'oneway' modifier did not participate in the type identity
// they would be a redeclaration. This first RUN checks they type-check without
// a redeclaration or an ambiguity error, and that 'nowait' and 'await' each
// pick their own overload without ambiguity
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -I %t 2>&1 %s -D ERRORS

// The second RUN checks that the two overloads, and their distributed thunks,
// mangle differently: the 'oneway' variant carries the 'Yo' function-type
// flavor operator (right after the 'Ya' async operator), the other one does
// not. It also checks that each call site, synthesized thunk and witness
// references the intended overload
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -I %t %s -module-name main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// A plain actor: 'oneway' participates in the type, so it distinguishes the
// overloads and their mangled symbols
actor Overloaded {
  func x() async {}
  func x() async oneway {}
}

// The 'async oneway' overload's symbol carries the 'Yo' operator immediately
// after 'Ya'; the plain 'async' overload's symbol does not
// CHECK-DAG: sil hidden{{.*}} @{{.*}}1x{{.*}}YaYo{{.*}}F
// CHECK-DAG: sil hidden{{.*}} @{{.*}}1x{{[^Y]*}}Ya{{[^Y]*}}F

// ==== -----------------------------------------------------------------------
// MARK: Distributed overloads which only differ in 'oneway'

distributed actor Greeter {
  distributed func hello() {}
  distributed func hello() oneway {}

  distributed func greet(_ n: Int) async {}
  distributed func greet(_ n: Int) async oneway {}

  // Only a 'oneway' overload
  distributed func onlyOneway() oneway {}
}

// The two distributed thunks have distinct names, and each one calls its own
// overload in its local branch
// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main7GreeterC5helloyyYaKFTE :
// CHECK: function_ref @$s4main7GreeterC5helloyyF :
// CHECK: } // end sil function '$s4main7GreeterC5helloyyYaKFTE'

// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main7GreeterC5helloyyYaYoKFTE :
// CHECK: function_ref @$s4main7GreeterC5helloyyYoF :
// CHECK: } // end sil function '$s4main7GreeterC5helloyyYaYoKFTE'

// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main7GreeterC5greetyySiYaKFTE :
// CHECK: function_ref @$s4main7GreeterC5greetyySiYaF :
// CHECK: } // end sil function '$s4main7GreeterC5greetyySiYaKFTE'

// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main7GreeterC5greetyySiYaYoKFTE :
// CHECK: function_ref @$s4main7GreeterC5greetyySiYaYoF :
// CHECK: } // end sil function '$s4main7GreeterC5greetyySiYaYoKFTE'

// 'await' calls the two-way overload
func callTwoWay(_ g: Greeter) async throws {
  try await g.hello()
  try await g.greet(1)
}
// CHECK-LABEL: sil hidden [ossa] @$s4main10callTwoWayyyAA7GreeterCYaKF :
// CHECK: function_ref @$s4main7GreeterC5helloyyYaKFTE :
// CHECK: function_ref @$s4main7GreeterC5greetyySiYaKFTE :
// CHECK: } // end sil function '$s4main10callTwoWayyyAA7GreeterCYaKF'

// 'nowait' calls the 'oneway' overload
func callOneway(_ g: Greeter) async throws {
  nowait g.hello()
  nowait g.greet(1)
}
// CHECK-LABEL: sil private [ossa] @$s4main10callOnewayyyAA7GreeterCYaKFyyYaKcfU_ :
// CHECK: function_ref @$s4main7GreeterC5helloyyYaYoKFTE :
// CHECK-LABEL: sil private [ossa] @$s4main10callOnewayyyAA7GreeterCYaKFyyYaKcfU0_ :
// CHECK: function_ref @$s4main7GreeterC5greetyySiYaYoKFTE :

func callOnlyOneway(_ g: Greeter) async throws {
  nowait g.onlyOneway()
#if ERRORS
  // Still diagnosed if there is no other overload
  // expected-error@+1{{call to 'oneway' distributed instance method 'onlyOneway()' must use 'nowait'}}
  try await g.onlyOneway()
#endif
}

// ==== -----------------------------------------------------------------------
// MARK: Through a distributed protocol

protocol Speaker: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func speak()
  distributed func speak() oneway
}

distributed actor Loud: Speaker {
  distributed func speak() {}
  distributed func speak() oneway {}
}
// The witnesses call their own overload, and the witnesses of the distributed
// thunk requirements call their own overload's distributed thunk
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4main4LoudCAA7SpeakerA2aDP5speakyyFTW :
// CHECK: class_method %0, #Loud.speak!distributed : (isolated Loud) -> () -> (),
// CHECK-LABEL: sil private [transparent] [distributed_thunk] [ossa] @$s4main4LoudCAA7SpeakerA2aDP5speakyyYaKFTWTE :
// CHECK: function_ref @$s4main4LoudC5speakyyYaKFTE :
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4main4LoudCAA7SpeakerA2aDP5speakyyYoFTW :
// CHECK: class_method %0, #Loud.speak!distributed : (isolated Loud) -> () oneway -> (),
// CHECK-LABEL: sil private [transparent] [distributed_thunk] [ossa] @$s4main4LoudCAA7SpeakerA2aDP5speakyyYaYoKFTWTE :
// CHECK: function_ref @$s4main4LoudC5speakyyYaYoKFTE :

func callSpeaker<S: Speaker>(_ s: S) async throws {
  try await s.speak()
  nowait s.speak()
}
// CHECK-LABEL: sil hidden [ossa] @$s4main11callSpeakeryyxYaKAA0C0RzlF :
// CHECK: witness_method $S, #Speaker.speak!distributed_thunk : <Self where Self : Speaker> (Self) -> () async throws -> ()
// CHECK: } // end sil function '$s4main11callSpeakeryyxYaKAA0C0RzlF'
// CHECK-LABEL: sil private [ossa] @$s4main11callSpeakeryyxYaKAA0C0RzlFyyYaKcfU_ :
// CHECK: witness_method $S, #Speaker.speak!distributed_thunk : <Self where Self : Speaker> (Self) -> () async throws oneway -> ()

// ==== -----------------------------------------------------------------------
// MARK: Plain actors and global actors

actor Worker {
  func work() {}
  func work() oneway {}
  func onlyOneway() oneway {}

  func selfCalls() async {
    work()
    // expected-warning@+1{{no 'async' operations occur within 'await' expression}}
    nowait work()
  }
}

func callWorker(_ w: Worker) async {
  await w.work()
  nowait w.work()

#if ERRORS
  // Still diagnosed if there is no other overload
  // expected-error@+1{{call to 'oneway' instance method 'onlyOneway()' must use 'nowait'}}
  await w.onlyOneway()
#endif
}
// CHECK-LABEL: sil hidden [ossa] @$s4main10callWorkeryyAA0C0CYaF :
// CHECK: class_method %0, #Worker.work : (isolated Worker) -> () -> (),
// CHECK: } // end sil function '$s4main10callWorkeryyAA0C0CYaF'
// CHECK-LABEL: sil private [ossa] @$s4main10callWorkeryyAA0C0CYaFyyYaKcfU_ :
// CHECK: class_method %0, #Worker.work : (isolated Worker) -> () oneway -> (),

@MainActor final class Screen {
  func draw() {}
  func draw() oneway {}
}

func callScreen(_ s: Screen) async {
  await s.draw()
  nowait s.draw()
}
// CHECK-LABEL: sil hidden [ossa] @$s4main10callScreenyyAA0C0CYaF :
// CHECK: function_ref @$s4main6ScreenC4drawyyF :
// CHECK: } // end sil function '$s4main10callScreenyyAA0C0CYaF'
// CHECK-LABEL: sil private [ossa] @$s4main10callScreenyyAA0C0CYaFyyYaKcfU_ :
// CHECK: function_ref @$s4main6ScreenC4drawyyYoF :

// A reference which is not a call picks the non-'oneway' overload too
@MainActor func unapplied(_ s: Screen) {
  let f = s.draw
  f()
}
// CHECK-LABEL: sil private [ossa] @$s4main9unappliedyyAA6ScreenCFyyScMYccADcfu_yyScMYccfu0_ :
// CHECK: function_ref @$s4main6ScreenC4drawyyF :

// Each requirement is witnessed by its own overload
// CHECK-LABEL: sil_witness_table hidden Loud: Speaker module main {
// CHECK-DAG: method #Speaker.speak!distributed: <Self where Self : Speaker> (isolated Self) -> () -> () : @$s4main4LoudCAA7SpeakerA2aDP5speakyyFTW
// CHECK-DAG: method #Speaker.speak!distributed_thunk: <Self where Self : Speaker> (Self) -> () async throws -> () : @$s4main4LoudCAA7SpeakerA2aDP5speakyyYaKFTWTE
// CHECK-DAG: method #Speaker.speak!distributed: <Self where Self : Speaker> (isolated Self) -> () oneway -> () : @$s4main4LoudCAA7SpeakerA2aDP5speakyyYoFTW
// CHECK-DAG: method #Speaker.speak!distributed_thunk: <Self where Self : Speaker> (Self) -> () async throws oneway -> () : @$s4main4LoudCAA7SpeakerA2aDP5speakyyYaYoKFTWTE
// CHECK: }
