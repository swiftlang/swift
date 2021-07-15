// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// rdar://77798215
// UNSUPPORTED: OS=windows-msvc

// REQUIRES: radar78290608

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SomeSpecificDistributedActor {
  distributed func hello() async throws -> String {
    "local impl"
  }
}

@available(SwiftStdlib 5.5, *)
extension SomeSpecificDistributedActor {

  @_dynamicReplacement(for: hello())
  nonisolated func _remote_hello() async throws -> String {
    return "remote impl (address: \(actor.id))"
  }
}

// ==== Fake Transport ---------------------------------------------------------

@available(SwiftStdlib 5.5, *)
struct FakeActorID: ActorIdentity {
  let id: UInt64
}

@available(SwiftStdlib 5.5, *)
enum FakeTransportError: ActorTransportError {
  case unsupportedActorIdentity(ActorIdentity)
}

@available(SwiftStdlib 5.5, *)
struct FakeTransport: ActorTransport {
  func resolve<Act>(address: ActorAddress, as actorType: Act.Type)
    throws -> ActorResolved<Act> where Act: DistributedActor {
    return .makeProxy
  }

  func assignIdentity<Act>(
    _ actorType: Act.Type
  ) -> ActorAddress where Act : DistributedActor {
    ActorAddress(parse: "")
  }

  public func actorReady<Act>(
    _ actor: Act
  ) where Act: DistributedActor {}

  public func resignIdentity(
    _ address: AnyActorIdentity
  ) {}

}

// ==== Execute ----------------------------------------------------------------

@_silgen_name("swift_distributed_actor_is_remote")
func __isRemoteActor(_ actor: AnyObject) -> Bool

func __isLocalActor(_ actor: AnyObject) -> Bool {
  return !__isRemoteActor(actor)
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test_remote() async {
  let address = ActorAddress(parse: "")
  let transport = FakeTransport()

  let local = SomeSpecificDistributedActor(transport: transport)
  _ = local.id
  assert(__isLocalActor(local) == true, "should be local")
  assert(__isRemoteActor(local) == false, "should be local")

  // assume it always makes a remote one
  let remote = try! SomeSpecificDistributedActor(resolve: address, using: transport)
  assert(__isLocalActor(remote) == false, "should be remote")
  assert(__isRemoteActor(remote) == true, "should be remote")

  print("done") // CHECK: done
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_remote()
  }
}

