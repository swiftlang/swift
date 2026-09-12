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
//     switch target.identifierByteCount { // check length first
//     case 38: // == "$e4main7GreeterC5hello4nameS2S_tYaKFTE".utf8.count, folded at compile time
//       if target.identifierEquals("$e4main7GreeterC5hello4nameS2S_tYaKFTE") { // then exact match
//         let arg0: String = try invocationDecoder.decodeNextArgument()
//         let result = try await self.hello(name: arg0)
//         try await resultHandler.onReturn(result)
//         return
//       }
//     default:
//       break
//     }
//     throw EmbeddedDistributedTargetNotFound(targetByteCount: target.identifierByteCount)
//   }
//
// The comparison goes through 'RemoteCallTarget.identifierEquals(_: StaticString)'
// in order to avoid relying on String in Embedded builds.
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

// It switches on the identifier's byte count and compares against it via
// 'identifierEquals', not through String's UTF8 view.
// CHECK: switch_stmt
// CHECK: member_ref_expr {{.*}}decl="Distributed.(file).RemoteCallTarget.identifierByteCount"
// CHECK: declref_expr {{.*}}decl="Distributed.(file).RemoteCallTarget.identifierEquals

// The matched branch: decode the argument, call the local impl, deliver the result.
// CHECK: decl="{{.*}}EmbeddedFakeInvocationDecoder extension.decodeNextArgument{{.*}}Argument -> String)]"
// CHECK: decl="{{.*}}Greeter.hello(name:)
// CHECK: decl="{{.*}}EmbeddedFakeResultHandler extension.onReturn{{.*}}Success -> String)]"

// The dispatch must not touch String's UTF8 view for the comparison.
// CHECK-NOT: decl="Swift.(file).String.UTF8View
