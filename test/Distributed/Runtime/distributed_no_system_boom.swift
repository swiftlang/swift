// RUN: %target-fail-simple-swift(-Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -parse-as-library %import-libdispatch) 2>&1 | %FileCheck %s
//
// // TODO: could not figure out how to use 'not --crash' it never is used with target-run-simple-swift
// This test is intended to *crash*, so we're using target-fail-simple-swift
// which expects the exit code of the program to be non-zero;
// We then check stderr for the expected error message using filecheck as usual.

// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// FIXME(distributed): remote functions dont seem to work on windows?
// XFAIL: OS=windows-msvc

import _Distributed

distributed actor SomeSpecificDistributedActor {
  distributed func hello() async throws -> String {
    "local impl"
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
  typealias Invocation = FakeInvocation
  typealias SerializationRequirement = Codable

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor,
            Act.ID == ActorID {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor {
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

  func makeInvocation() -> Invocation {
    .init()
  }
}

struct FakeInvocation: DistributedTargetInvocation {
  typealias ArgumentDecoder = FakeArgumentDecoder
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(mangledType: T.Type) throws {}
  mutating func recordArgument<Argument: SerializationRequirement>(argument: Argument) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(mangledType: R.Type) throws {}
  mutating func recordErrorType<E: Error>(mangledType: E.Type) throws {}
  mutating func doneRecording() throws {}

  // === Receiving / decoding -------------------------------------------------

  mutating func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  mutating func argumentDecoder() -> FakeArgumentDecoder { .init() }
  mutating func decodeReturnType() throws -> Any.Type? { nil }
  mutating func decodeErrorType() throws -> Any.Type? { nil }

  struct FakeArgumentDecoder: DistributedTargetInvocationArgumentDecoder {
    typealias SerializationRequirement = Codable
  }
}

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

func test_remote() async {
  let address = ActorAddress(parse: "")
  let system = FakeActorSystem()

  let remote = try! SomeSpecificDistributedActor.resolve(id: address, using: system)
  _ = try! await remote.hello() // let it crash!

  // CHECK: SOURCE_DIR/test/Distributed/Runtime/distributed_no_system_boom.swift:{{[0-9]+}}: Fatal error: Invoked remote placeholder function '_remote_hello' on remote distributed actor of type 'main.SomeSpecificDistributedActor'. Configure an appropriate 'DistributedActorSystem' for this actor to resolve this error (e.g. by depending on some specific actor system library).
}

@main struct Main {
  static func main() async {
    await test_remote()
  }
}

