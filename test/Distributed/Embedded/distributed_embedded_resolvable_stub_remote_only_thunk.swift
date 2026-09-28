// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 -plugin-path %swift-plugin-dir -module-name main %s -o %t/main.sil
// RUN: %FileCheck %s --check-prefix=SIL < %t/main.sil
// RUN: %FileCheck %s --check-prefix=NO-STUB-FATAL-ERROR < %t/main.sil
// RUN: %FileCheck %s --check-prefix=DISPATCH < %t/main.sil
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 -plugin-path %swift-plugin-dir -module-name main %s -o %t/main.ll
// RUN: %FileCheck %s --check-prefix=IR < %t/main.ll

// REQUIRES: OS=macosx
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: optimized_stdlib

// In Embedded Swift the distributed thunks of a '@Resolvable' stub '$Greeter'
// only keep the remote branch and trap with 'fatalError()' if the stub is
// local, so they do not call the stub body or suspend on a local call

import _Concurrency
import Distributed

public struct MyActorID: Sendable, Hashable {
  public let id: UInt64
}

// The system's serialization requirement, without any requirements
public protocol MySerializationRequirement {}
extension String: MySerializationRequirement {}

public struct MyEncoder: DistributedTargetInvocationEncoder {
  public init() {}
  public mutating func doneRecording() throws {}
}
extension MyEncoder {
  public mutating func recordArgument<Value: MySerializationRequirement>(
      _ argument: RemoteCallArgument<Value>) throws {}
}

public struct MyDecoder: DistributedTargetInvocationDecoder {
  public init() {}
}
extension MyDecoder {
  public mutating func decodeNextArgument<Argument: MySerializationRequirement>() throws -> Argument {
    fatalError()
  }
}

public struct MyResultHandler: DistributedTargetInvocationResultHandler {
  public init() {}
  public func onReturnVoid() async throws {}
  public func onThrow(error: any Error) async throws {}
}
extension MyResultHandler {
  public func onReturn<Success: MySerializationRequirement>(value: Success) async throws {}
}

public final class MySystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = MyActorID
  public typealias SerializationRequirement = MySerializationRequirement
  public typealias InvocationEncoder = MyEncoder
  public typealias InvocationDecoder = MyDecoder
  public typealias ResultHandler = MyResultHandler

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ID == ActorID { return nil }
  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ID == ActorID { return MyActorID(id: 0) }
  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {}
  public func resignID(_ id: ActorID) {}

  public func makeInvocationEncoder() -> InvocationEncoder { .init() }

  public func remoteCall<Act, Err, Res>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type, returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID,
            Err: Error, Res: MySerializationRequirement { fatalError() }

  public func remoteCallVoid<Act, Err>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error { fatalError() }
}

@Resolvable
public protocol Greeter: DistributedActor where ActorSystem == MySystem {
  distributed func greet(name: String) -> String
  distributed func ping()
}

public distributed actor GreeterImpl: Greeter {
  public typealias ActorSystem = MySystem

  public distributed func greet(name: String) -> String {
    "Hello, \(name)!"
  }

  public distributed func ping() {}
}

public func callGreet(_ greeter: any Greeter) async throws -> String {
  try await greeter.greet(name: "any")
}

// ==== ------------------------------------------------------------------------
// MARK: Stub thunks only keep the remote branch

// The Embedded stub thunks trap without the message formatting of
// _distributedStubFatalError
// NO-STUB-FATAL-ERROR-NOT: _distributedStubFatalError

// SIL-LABEL: sil [thunk] @$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5greet4nameS2S_tYaKFTE{{.*}} :
// SIL: function_ref @swift_distributed_actor_is_remote
// SIL-NOT: function_ref @$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5greet4nameS2S_tF
// SIL: function_ref @$es31_embeddedReportFatalErrorInFile
// SIL-NOT: function_ref @$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5greet4nameS2S_tF
// SIL: function_ref @$e4main8MySystemC10remoteCall2on
// SIL-NOT: function_ref @$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5greet4nameS2S_tF
// SIL: } // end sil function '$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5greet4nameS2S_tYaKFTE{{.*}}'

// SIL-LABEL: sil [thunk] @$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE4pingyyYaKFTE{{.*}} :
// SIL: function_ref @swift_distributed_actor_is_remote
// SIL-NOT: function_ref @$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE4pingyyF
// SIL: function_ref @$es31_embeddedReportFatalErrorInFile
// SIL-NOT: function_ref @$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE4pingyyF
// SIL: function_ref @$e4main8MySystemC14remoteCallVoid2on
// SIL-NOT: function_ref @$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE4pingyyF
// SIL: } // end sil function '$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE4pingyyYaKFTE{{.*}}'

// Nothing refers to the stub bodies anymore, so they are dead stripped
// IR-DAG: @"$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5greet4nameS2S_tF" = alias void (), ptr @_swift_dead_method_stub
// IR-DAG: @"$e4main7GreeterPAA11Distributed01_C9ActorStubRzrlE4pingyyF" = alias void (), ptr @_swift_dead_method_stub

// ==== ------------------------------------------------------------------------
// MARK: Regular distributed actor thunks keep the local branch

// SIL-LABEL: sil [thunk] [distributed] @$e4main11GreeterImplC5greet4nameS2S_tYaKFTE :
// SIL: function_ref @swift_distributed_actor_is_remote
// SIL: function_ref @$e4main11GreeterImplC5greet4nameS2S_tF
// SIL: } // end sil function '$e4main11GreeterImplC5greet4nameS2S_tYaKFTE'

// ==== ------------------------------------------------------------------------
// MARK: Stub dispatcher has no targets

// A stub is never local, so its '_executeDistributedTarget' never matches a
// target and does not decode arguments or reach the stub thunks
// DISPATCH-LABEL: sil @$e4main8$GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandler{{.*}} :
// DISPATCH-NOT: function_ref @$e4main9MyDecoderV18decodeNextArgument
// DISPATCH-NOT: function_ref @$e4main15MyResultHandlerV
// DISPATCH-NOT: function_ref @$e4main7GreeterPAA11Distributed01_C9ActorStub
// DISPATCH: function_ref @$e11Distributed08EmbeddedA14TargetNotFoundV15targetByteCount
// DISPATCH-NOT: function_ref @$e4main9MyDecoderV18decodeNextArgument
// DISPATCH-NOT: function_ref @$e4main15MyResultHandlerV
// DISPATCH-NOT: function_ref @$e4main7GreeterPAA11Distributed01_C9ActorStub
// DISPATCH: } // end sil function '$e4main8$GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandler{{.*}}'

// The dispatcher of the real actor still decodes and calls its targets
// DISPATCH-LABEL: sil @$e4main11GreeterImplC25_executeDistributedTarget6target17invocationDecoder13resultHandler{{.*}} :
// DISPATCH: function_ref @$e4main9MyDecoderV18decodeNextArgument
// DISPATCH: } // end sil function '$e4main11GreeterImplC25_executeDistributedTarget6target17invocationDecoder13resultHandler{{.*}}'
