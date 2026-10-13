// RUN: %target-swift-frontend -emit-silgen -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 -plugin-path %swift-plugin-dir -module-name main %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: optimized_stdlib

// A call can name 'Greeter.hello(name:)' by the actor's own thunk or by the
// '$Greeting' stub's thunk. Both names reach the same implementation, so the
// synthesized dispatch matches them in one branch and calls it once.

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

@Resolvable
protocol Greeting: DistributedActor where ActorSystem == EmbeddedFakeRoundtripActorSystem {
  distributed func hello(name: String) -> String
}

distributed actor Greeter: Greeting {
  distributed func hello(name: String) -> String {
    return "Hello, \(name)!"
  }
}

// CHECK-LABEL: sil hidden [ossa] @$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF :
// CHECK: string_literal utf8 "$e4main7GreeterC5hello4nameS2S_tYaKFTE"
// CHECK: function_ref static Bool.|| infix(_:_:)
// CHECK: function_ref Greeter.hello(name:)
// CHECK-NOT: function_ref Greeter.hello(name:)
// CHECK: } // end sil function '$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF'

// The second name is matched in the autoclosure on the right of '||'.
// CHECK-LABEL: sil private [transparent] [ossa] @$e4main7GreeterC25_executeDistributedTarget{{.*}}fu_ :
// CHECK: string_literal utf8 "$e4main9$GreetingC5hello4nameS2S_tYaKFTE"
