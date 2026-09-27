// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out.sil
// RUN: %FileCheck %s < %t/out.sil
// RUN: %FileCheck %s --check-prefix=DISPATCH < %t/out.sil

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: swift_feature_OnewayNowait
// REQUIRES: optimized_stdlib

// In Embedded Swift 'distributed func hello()' and
// 'distributed func hello() oneway' are distinct overloads with distinct
// distributed thunks: the two-way one is 'async throws', the 'oneway' one is
// synchronous and mangled with 'Yo'. 'try await' calls the two-way thunk,
// 'try nowait' calls the 'oneway' thunk, each thunk calls its own overload, and
// the receive dispatcher routes each remote call target to its own overload
// The same holds for a plain actor

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  distributed func hello() { print("two-way") }
  distributed func hello() oneway { print("oneway") }
}

// CHECK-LABEL: sil [thunk] [distributed] @$e4main7GreeterC5helloyyYaKFTE : $@convention(method) @async (@guaranteed Greeter) -> @error any Error {
// CHECK: function_ref @$e4main7GreeterC5helloyyF :
// CHECK: } // end sil function '$e4main7GreeterC5helloyyYaKFTE'

// The local branch of the 'oneway' thunk enqueues a call of the 'oneway'
// overload
// CHECK-LABEL: sil [thunk] [distributed] @$e4main7GreeterC5helloyyYoKFTE : $@convention(method) (@sil_isolated @guaranteed Greeter) -> @error any Error {
// CHECK: function_ref @$e4main7GreeterC5helloyyYoKFyACYicfU_ :
// CHECK: } // end sil function '$e4main7GreeterC5helloyyYoKFTE'
// CHECK-LABEL: sil @$e4main7GreeterC5helloyyYoKFyACYicfU_ :
// CHECK: function_ref @$e4main7GreeterC5helloyyYoF :
// CHECK: } // end sil function '$e4main7GreeterC5helloyyYoKFyACYicfU_'

func sendTwoWay(_ g: Greeter) async throws {
  try await g.hello()
}
// CHECK-LABEL: sil @$e4main10sendTwoWayyyAA7GreeterCYaKF :
// CHECK: function_ref @$e4main7GreeterC5helloyyYaKFTE :
// CHECK: } // end sil function '$e4main10sendTwoWayyyAA7GreeterCYaKF'

func sendOneway(_ g: Greeter) throws {
  try nowait g.hello()
}
// CHECK-LABEL: sil @$e4main10sendOnewayyyAA7GreeterCKF :
// CHECK: function_ref @$e4main7GreeterC5helloyyYoKFTE :
// CHECK: } // end sil function '$e4main10sendOnewayyyAA7GreeterCKF'

actor Plain {
  func f() {}
  func f() oneway {}
}

func sendPlain(_ p: Plain) async {
  await p.f()
  nowait p.f()
}
// CHECK-LABEL: sil @$e4main9sendPlainyyAA0C0CYaF :
// CHECK: function_ref @$e4main5PlainC1fyyF :
// CHECK: function_ref @$e4main9sendPlainyyAA0C0CYaFyADYicfU_ :
// CHECK: } // end sil function '$e4main9sendPlainyyAA0C0CYaF'
// CHECK-LABEL: sil @$e4main9sendPlainyyAA0C0CYaFyADYicfU_ :
// CHECK: function_ref @$e4main5PlainC1fyyYoF :

// The dispatcher has an arm for each of the two targets: the two-way arm
// calls the two-way thunk, the 'oneway' arm enqueues a call of the 'oneway'
// overload
// DISPATCH-LABEL: sil @$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF :
// DISPATCH: string_literal utf8 "$e4main7GreeterC5helloyyYaKFTE"
// DISPATCH: function_ref @$e4main7GreeterC5helloyyYaKFTE :
// DISPATCH: string_literal utf8 "$e4main7GreeterC5helloyyYoKFTE"
// DISPATCH: function_ref @$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKFyACYicfU_ :
// DISPATCH: } // end sil function '$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF'
// DISPATCH-LABEL: sil @$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKFyACYicfU_ :
// DISPATCH: function_ref @$e4main7GreeterC5helloyyYoF :

@main struct Main {
  static func main() async {
    let system = EmbeddedFakeRoundtripActorSystem()
    let greeter = Greeter(actorSystem: system)
    try? await sendTwoWay(greeter)
    try? sendOneway(greeter)
    await sendPlain(Plain())
  }
}
