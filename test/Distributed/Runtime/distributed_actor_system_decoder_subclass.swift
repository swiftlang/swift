// RUN: %target-run-simple-swift( -target %target-swift-5.7-abi-triple -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor DA {
  distributed func test() {}
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  // CHECK: assign type:DA, address:ActorAddress(address: "xxx")
  // CHECK: ready actor:main.DA, address:ActorAddress(address: "xxx")
  let da = DA(actorSystem: system)
  try await da.test()
  // CHECK: resign address:ActorAddress(address: "xxx")
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}


// ==== Fake Transport ---------------------------------------------------------

struct ActorAddress: Hashable, Sendable, Codable {
  let address: String
  init(parse address : String) {
    self.address = address
  }

  // Explicit implementations to make our TestEncoder/Decoder simpler
  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    self.address = try container.decode(String.self)
    print("decode ActorAddress -> \(self)")
  }

  func encode(to encoder: Encoder) throws {
    print("encode \(self)")
    var container = encoder.singleValueContainer()
    try container.encode(self.address)
  }
}

final class FakeActorSystem: DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocation
  typealias InvocationEncoder = FakeInvocation
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
    where Act: DistributedActor,
    Act.ID == ActorID  {
    print("resolve type:\(actorType), address:\(id)")
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor, Act.ID == ActorID {
    let address = ActorAddress(parse: "xxx")
    print("assign type:\(actorType), address:\(address)")
    return address
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
    print("ready actor:\(actor), address:\(actor.id)")
  }

  func resignID(_ id: ActorID) {
    print("resign address:\(id)")
  }

  func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error,
    Res: SerializationRequirement {
    fatalError("not implemented: \(#function)")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    fatalError("not implemented: \(#function)")
  }

}

class EncoderBase: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Codable

  func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  func recordErrorType<E: Error>(_ type: E.Type) throws {}
  func doneRecording() throws {}

}

class DecoderBase: EncoderBase, DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  func decodeReturnType() throws -> Any.Type? { nil }
  func decodeErrorType() throws -> Any.Type? { nil }
}

class FakeInvocation: DecoderBase {}

@available(SwiftStdlib 5.5, *)
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
