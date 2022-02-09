// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: windows

import _Distributed

distributed actor Capybara {
  // only the local capybara can do this!
  func eat() -> String {
    "watermelon"
  }
}


// ==== Fake Transport ---------------------------------------------------------
struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address: String) {
    self.address = address
  }
}

struct FakeActorSystem: DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocation
  typealias InvocationEncoder = FakeInvocation
  typealias SerializationRequirement = Codable

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
  throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
          where Act: DistributedActor {
    let id = ActorAddress(parse: "xxx")
    return id
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
  }

  func resignID(_ id: ActorID) {
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
    Err: Error,
//          Act.ID == ActorID,
    Res: SerializationRequirement {
    fatalError("Not implemented")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
    Err: Error
//          Act.ID == ActorID
  {
    fatalError("Not implemented")
  }
}

class FakeInvocation: DistributedTargetInvocationEncoder, DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  func recordArgument<Argument: SerializationRequirement>(_ argument: Argument) throws {}
  func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  func recordErrorType<E: Error>(_ type: E.Type) throws {}
  func doneRecording() throws {}

  // === Receiving / decoding -------------------------------------------------

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  func decodeReturnType() throws -> Any.Type? { nil }
  func decodeErrorType() throws -> Any.Type? { nil }
}

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Capybara(system: system)
  // await local.eat() // SHOULD ERROR
  let valueWhenLocal: String? = await local.whenLocal { __secretlyKnownToBeLocal in
    __secretlyKnownToBeLocal.eat()
  }

  // CHECK: valueWhenLocal: watermelon
  print("valueWhenLocal: \(valueWhenLocal ?? "nil")")

  let remote = try Capybara.resolve(id: local.id, using: system)
  let valueWhenRemote: String? = await remote.whenLocal { __secretlyKnownToBeLocal in
    __secretlyKnownToBeLocal.eat()
  }
  
  // CHECK: valueWhenRemote: nil
  print("valueWhenRemote: \(valueWhenRemote ?? "nil")")
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
