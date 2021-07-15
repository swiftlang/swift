// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: radar78290608

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SomeSpecificDistributedActor {

  distributed func hello() async throws {
     print("hello from \(self.id)")
  }
}

// ==== Execute ----------------------------------------------------------------

@_silgen_name("swift_distributed_actor_is_remote")
func __isRemoteActor(_ actor: AnyObject) -> Bool

func __isLocalActor(_ actor: AnyObject) -> Bool {
  return !__isRemoteActor(actor)
}

// ==== Fake Transport ---------------------------------------------------------

@available(SwiftStdlib 5.5, *)
struct ActorAddress: ActorIdentity {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

@available(SwiftStdlib 5.5, *)
struct FakeTransport: ActorTransport {

  func decodeIdentity(from decoder: Decoder) throws -> AnyActorIdentity {
    fatalError()
  }

  func resolve<Act>(identity: Act.ID, as actorType: Act.Type)
    throws -> ActorResolved<Act>
      where Act: DistributedActor {
    fatalError()
  }

  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    ActorAddress(parse: "")
  }

  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor {
    fatalError()
  }

  func resignIdentity(_ id: AnyActorIdentity) {}
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test_initializers() {
  let address = ActorAddress(parse: "")
  let transport = FakeTransport()

  _ = SomeSpecificDistributedActor(transport: transport)
  _ = try! SomeSpecificDistributedActor.resolve(address, using: transport)
}

@available(SwiftStdlib 5.5, *)
func test_address() {
  let transport = FakeTransport()

  let actor = SomeSpecificDistributedActor(transport: transport)
  _ = actor.id
}

@available(SwiftStdlib 5.5, *)
func test_run(transport: FakeTransport) async {
  let actor = SomeSpecificDistributedActor(transport: transport)

  print("before") // CHECK: before
  try! await actor.hello()
  print("after") // CHECK: after
}

@available(SwiftStdlib 5.5, *)
func test_echo(transport: FakeTransport) async {
  let actor = SomeSpecificDistributedActor(transport: transport)

  let echo = try! await actor.echo(int: 42)
  print("echo: \(echo)") // CHECK: echo: 42
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_run(transport: FakeTransport())
    await test_echo(transport: FakeTransport())
  }
}
