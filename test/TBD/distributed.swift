// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// REQUIRES: distributed

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-testing -disable-availability-checking -emit-ir -o %t/test.ll -emit-tbd -emit-tbd-path %t/test.tbd -I %t -tbd-install_name distributed
// RUN %llvm-nm -g %t/test.tbd | %FileCheck %s --dump-input=always

import Distributed

// CHECK: @"$s4test1AC13_remote_helloyyYaKFTE" = hidden global %swift.async_func_pointer
// CHECK: @"$s4test1AC13_remote_helloyyYaKFTETu" = hidden global %swift.async_func_pointer
distributed actor SomeDistributedActor {
  typealias ActorSystem = FakeActorSystem
  distributed func hello(name: String) -> String {
    "Hello, \(name)!"
  }
}

// function:
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tF
// function method descriptor
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tFTq
// thunk, method reference
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tFTE
// thunk, method reference + async function pointer
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tFTETu

// ==== Fake Address -----------------------------------------------------------

public struct ActorAddress: Hashable, Sendable, Codable {
  public let address: String
  public init(parse address : String) {
    self.address = address
  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    self.address = try container.decode(String.self)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(self.address)
  }
}

// ==== Fake Transport ---------------------------------------------------------

public struct FakeActorSystem: DistributedActorSystem {
  public typealias ActorID = ActorAddress
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeResultHandler

  init() {
    print("Initialized new FakeActorSystem")
  }

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor,
      Act.ID == ActorID  {
    nil
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor,
      Act.ID == ActorID {
    ActorAddress(parse: "xxx")
  }

  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
  }

  public func resignID(_ id: ActorID) {
  }

  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing errorType: Err.Type,
    returning returnType: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    print("remoteCall: on:\(actor), target:\(target), invocation:\(invocationEncoder), throwing:\(errorType), returning:\(returnType)")
    return "<REMOTE CALL>" as! Res
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing errorType: Err.Type
  ) async throws
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error {
    print("remoteCallVoid: on:\(actor), target:\(target), invocation:\(invocationEncoder), throwing:\(errorType)")
    return ()
  }
}

// === Sending / encoding -------------------------------------------------
public struct FakeInvocationEncoder: DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Codable

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  public mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  public mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  public mutating func doneRecording() throws {}
}

// === Receiving / decoding -------------------------------------------------
public class FakeInvocationDecoder : DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Codable

  public func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  public func decodeReturnType() throws -> Any.Type? { nil }
  public func decodeErrorType() throws -> Any.Type? { nil }
}

public struct FakeResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Codable

  public func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    fatalError("Not implemented: \(#function)")
  }

  public func onReturnVoid() async throws {
    fatalError("Not implemented: \(#function)")
  }

  public func onThrow<Err: Error>(error: Err) async throws {
    fatalError("Not implemented: \(#function)")
  }
}
