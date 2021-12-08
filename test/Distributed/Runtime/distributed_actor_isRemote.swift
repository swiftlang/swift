// RUN: %target-run-simple-swift(-Onone -Xfrontend -g -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

@available(SwiftStdlib 5.6, *)
distributed actor SomeSpecificDistributedActor {
  distributed func hello() async throws -> String {
    "local impl"
  }
}

@available(SwiftStdlib 5.6, *)
extension SomeSpecificDistributedActor {

  @_dynamicReplacement(for: _remote_hello())
  nonisolated func _remote_impl_hello() async throws -> String {
    return "remote impl (address: \(self.id))"
  }
}

// ==== Fake Transport ---------------------------------------------------------

@available(SwiftStdlib 5.6, *)
struct FakeActorID: Sendable, Hashable, Codable {
  let id: UInt64
}

@available(SwiftStdlib 5.6, *)
enum FakeActorSystemError: DistributedActorSystemError {
  case unsupportedActorIdentity(AnyActorIdentity)
}

@available(SwiftStdlib 5.6, *)
struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

@available(SwiftStdlib 5.6, *)
struct FakeActorSystem: DistributedActorSystem {

  func resolve<Act>(id: ID, as actorType: Act.Type)
  throws -> Act?
      where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    let id = ActorAddress(parse: "xxx")
    print("assignID type:\(actorType), id:\(id)")
    return .init(id)
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
    print("actorReady actor:\(actor), id:\(actor.id)")
  }

  func resignID(_ id: AnyActorIdentity) {
    print("assignID id:\(id)")
  }
}

@available(SwiftStdlib 5.6, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

@_silgen_name("swift_distributed_actor_is_remote")
func __isRemoteActor(_ actor: AnyObject) -> Bool

func __isLocalActor(_ actor: AnyObject) -> Bool {
  return !__isRemoteActor(actor)
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.6, *)
func test_remote() async {
  let address = ActorAddress(parse: "sact://127.0.0.1/example#1234")
  let transport = FakeActorSystem()

  let local = SomeSpecificDistributedActor(transport: transport)
  assert(__isLocalActor(local) == true, "should be local")
  assert(__isRemoteActor(local) == false, "should be local")
  print("isRemote(local) = \(__isRemoteActor(local))") // CHECK: isRemote(local) = false
  print("local.id = \(local.id)") // CHECK: local.id = AnyActorIdentity(ActorAddress(address: "xxx"))
  print("local.transport = \(local.actorSystem)") // CHECK: local.transport = FakeActorSystem()

  // assume it always makes a remote one
  let remote = try! SomeSpecificDistributedActor.resolve(.init(address), using: transport)
  assert(__isLocalActor(remote) == false, "should be remote")
  assert(__isRemoteActor(remote) == true, "should be remote")
  print("isRemote(remote) = \(__isRemoteActor(remote))") // CHECK: isRemote(remote) = true

  // Check the id and transport are the right values, and not trash memory
  print("remote.id = \(remote.id)") // CHECK: remote.id = AnyActorIdentity(ActorAddress(address: "sact://127.0.0.1/example#1234"))
  print("remote.transport = \(remote.actorSystem)") // CHECK: remote.transport = FakeActorSystem()

  print("done") // CHECK: done
}

@available(SwiftStdlib 5.6, *)
@main struct Main {
  static func main() async {
    await test_remote()
  }
}

