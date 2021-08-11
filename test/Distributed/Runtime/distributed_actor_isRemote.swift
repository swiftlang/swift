// RUN: %target-run-simple-swift(-Onone -Xfrontend -g -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UN_SUPPORTED: use_os_stdlib
// UN_SUPPORTED: back_deployment_runtime

// rdar://77798215
// UN_SUPPORTED: OS=windows-msvc

// RE_QUIRES: rdar78290608

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SomeSpecificDistributedActor {
  distributed func hello() async throws -> String {
    "local impl"
  }
}

@available(SwiftStdlib 5.5, *)
extension SomeSpecificDistributedActor {

  @_dynamicReplacement(for: _remote_hello())
  nonisolated func _remote_impl_hello() async throws -> String {
    return "remote impl (address: \(self.id))"
  }
}

// ==== Fake Transport ---------------------------------------------------------

@available(SwiftStdlib 5.5, *)
struct FakeActorID: ActorIdentity {
  let id: UInt64
}

@available(SwiftStdlib 5.5, *)
enum FakeTransportError: ActorTransportError {
  case unsupportedActorIdentity(AnyActorIdentity)
}

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
    fatalError("not implemented:\(#function)")
  }

  func resolve<Act>(_ identity: AnyActorIdentity, as actorType: Act.Type)
  throws -> ActorResolved<Act>
      where Act: DistributedActor {
    .makeProxy
  }

  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    let id = ActorAddress(parse: "xxx")
    print("assign type:\(actorType), id:\(id)")
    return .init(id)
  }

  func actorReady<Act>(_ actor: Act) where Act: DistributedActor {
    print("ready actor:\(actor), id:\(actor.id)")
  }

  func resignIdentity(_ id: AnyActorIdentity) {
    print("resign id:\(id)")
  }
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
  let address = ActorAddress(parse: "sact://127.0.0.1/example#1234")
  let transport = FakeTransport()

  print("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")
//  let local = SomeSpecificDistributedActor(transport: transport)
//  assert(__isLocalActor(local) == true, "should be local")
//  assert(__isRemoteActor(local) == false, "should be local")
//  print("local.id = \(local.id)") // CHECK: NEIN
//  assert(AnyActorIdentity(address) == local.id)

  // assume it always makes a remote one
  let remote = try! SomeSpecificDistributedActor.resolve(.init(address), using: transport)
//  assert(__isLocalActor(remote) == false, "should be remote")
//  assert(__isRemoteActor(remote) == true, "should be remote")

  // Check the id and transport are the right values, and not trash memory
  print("remote.id = \(remote.id)") // CHECK: NEIN
  print("remote.transport = \(remote.actorTransport)") // CHECK: NEIN
//  assert(AnyActorIdentity(address) == remote.id)

  print("done") // CHECK: done
  let xxx = remote
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_remote()
  }
}

