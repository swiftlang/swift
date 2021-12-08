// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

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

struct FakeActorSystem: DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias SerializationRequirement = Codable

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ID == ActorID  {
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

  // ==== ----------------------------------------------------------------------------------------------------------------

  @inlinable
  func makeInvocationArguments() throws -> some DistributedFuncInvocationArguments {
    let buffer = FakeNIOByteBuffer()
    return FakeNIODistributedFuncInvocationArguments(buffer: buffer)
  }

  @usableFromInline
  struct FakeNIODistributedFuncInvocationArguments: DistributedFuncInvocationArguments {
    let buffer = FakeNIOByteBuffer()

    mutating func recordArgument<Argument>(argument: Argument) throws {

    }

    mutating func recordGenericSubstitution(i: Int, j: Int, mangledTypeName: String) throws

    mutating func recordResultType(mangledTypeName: String) throws // TODO: result type, return type?

    mutating func recordErrorType(mangledTypeName: String) throws

  }
  struct FakeNIOByteBuffer {

  }

  func remoteCall<Act, Args, Err, Res>(
      on actor: Act,
      method: DistributedMethodName,
      arguments: Args,
      throwing: Err.Type,
      returning: Res.Type
  ) async throws -> Res.Type
      where Act: DistributedActor,
      Act.ID == ActorID,
      Args: DistributedFuncInvocationArguments {

  }
}

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem
