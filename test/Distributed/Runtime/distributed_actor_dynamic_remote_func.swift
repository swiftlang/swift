// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

distributed actor LocalWorker {
  typealias Transport = FakeActorSystem

  init(system: FakeActorSystem) {}

  distributed func function() async throws -> String {
    "local:"
  }

  distributed func echo(name: String) async throws -> String {
    "local:\(name)"
  }
}

// ==== Fake Transport ---------------------------------------------------------

struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

struct FakeActorSystem: DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocation
  typealias InvocationEncoder = FakeInvocation
  typealias SerializationRequirement = Codable

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor,
      Act.ID == ActorID {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor,
      Act.ID == ActorID {
    let id = ActorAddress(parse: "xxx")
    print("assign type:\(actorType), id:\(id)")
    return id
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
    print("ready actor:\(actor), id:\(actor.id)")
  }

  func resignID(_ id: ActorID) {
    print("ready id:\(id)")
  }

  func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocationDecoder: inout InvocationDecoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
//          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    return "remoteCall: \(target.mangledName)" as! Res
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocationDecoder: inout InvocationDecoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
//          Act.ID == ActorID,
          Err: Error {
    fatalError("not implemented \(#function)")
  }
}

struct FakeInvocation: DistributedTargetInvocationEncoder, DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  // FIXME(distributed): must support trivial types
  var xx: [String] = []
  var x2x: [String] = []
  var x3x: [String] = []
  var x4x: [String] = []

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Argument: SerializationRequirement>(_ argument: Argument) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}

  // === Receiving / decoding -------------------------------------------------

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  mutating func decodeNextArgument<Argument>(
    _ argumentType: Argument.Type,
    into pointer: UnsafeMutablePointer<Argument> // pointer to our hbuffer
  ) throws { /* ... */ }
  func decodeReturnType() throws -> Any.Type? { nil }
  func decodeErrorType() throws -> Any.Type? { nil }

  struct FakeArgumentDecoder: DistributedTargetInvocationArgumentDecoder {
    typealias SerializationRequirement = Codable
  }
}


@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

func test_local() async throws {
  let system = FakeActorSystem()

  let worker = LocalWorker(system: system)
  let x = try await worker.function()
  print("call: \(x)")
  // CHECK: assign type:LocalWorker, id:[[ADDRESS:.*]]
  // CHECK: ready actor:main.LocalWorker, id:[[ADDRESS]]
  // CHECK: call: local:
}

func test_remote() async throws {
  let address = ActorAddress(parse: "")
  let system = FakeActorSystem()

  let worker = try LocalWorker.resolve(id: address, using: system)
  let x = try await worker.function()
  print("call: \(x)")
  // CHECK: remoteCall: $s4main11LocalWorkerC8functionSSyYaKFTE

  let e = try await worker.echo(name: "Charlie")
  print("call: \(e)")
  // CHECK: remoteCall: $s4main11LocalWorkerC4echo4nameS2S_tYaKFTE
}

@main struct Main {
  static func main() async {
    try! await test_local()
    try! await test_remote()
  }
}
