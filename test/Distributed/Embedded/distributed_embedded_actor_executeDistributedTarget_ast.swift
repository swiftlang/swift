// RUN: %target-swift-frontend -dump-ast -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 -module-name main %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift 2>&1 | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

// Pseudo code of the synthesized func:
//
//   nonisolated func _executeDistributedTarget(
//     target: RemoteCallTarget,
//     invocationDecoder: inout ActorSystem.InvocationDecoder,
//     resultHandler: ActorSystem.ResultHandler
//   ) async throws {
//     switch target.identifier.utf8.count {   // length first, then compare bytes
//     case 38: // == "$e4main7GreeterC5hello4nameS2S_tYaKFTE".utf8.count, folded at compile time
//       if target.identifier.utf8.elementsEqual("$e4main7GreeterC5hello4nameS2S_tYaKFTE".utf8) {
//         let arg0: String = try invocationDecoder.decodeNextArgument()
//         let result = try await self.hello(name: arg0)
//         try await resultHandler.onReturn(result)
//         return
//       }
//     default:
//       break
//     }
//     throw EmbeddedDistributedTargetNotFound(target: target.identifier)
//   }
//
// The actor system lives in the shared Runtime/Inputs/EmbeddedFakeActorSystem.swift.

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  distributed func hello(name: String) -> String {
    return "Hello, \(name)!"
  }
}

// The synthesized dispatch method.
// CHECK: func_decl {{.*}}"_executeDistributedTarget(target:invocationDecoder:resultHandler:)"

// It switches on, and compares against, the incoming target identifier.
// CHECK: switch_stmt
// CHECK: member_ref_expr {{.*}}decl="Distributed.(file).RemoteCallTarget.identifier"

// The matched branch: decode the argument, call the local impl, deliver the result.
// CHECK: decl="{{.*}}EmbeddedFakeInvocationDecoder extension.decodeNextArgument{{.*}}Argument -> String)]"
// CHECK: decl="{{.*}}Greeter.hello(name:)
// CHECK: decl="{{.*}}EmbeddedFakeResultHandler extension.onReturn{{.*}}Success -> String)]"
