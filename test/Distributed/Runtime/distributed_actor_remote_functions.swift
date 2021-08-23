// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// rdar://77798215
// UNSUPPORTED: OS=windows-msvc

// REQUIRES: rdar78290608

import _Distributed
import _Concurrency

struct Boom: Error {
  let whoFailed: String
  init(_ whoFailed: String) {
    self.whoFailed = whoFailed
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor SomeSpecificDistributedActor {
  let state: String = "hi there"

  distributed func helloAsyncThrows() async throws -> String {
    "local(\(#function))"
  }

  distributed func helloAsync() async -> String {
   "local(\(#function))"
  }

  distributed func helloThrows() throws -> String {
   "local(\(#function))"
  }

  distributed func hello() -> String {
   "local(\(#function))"
  }

  // === errors

  distributed func helloThrowsImplBoom() throws -> String {
    throw Boom("impl")
  }

  distributed func helloThrowsTransportBoom() throws -> String {
    "local(\(#function))"
  }
}

@available(SwiftStdlib 5.5, *)
extension SomeSpecificDistributedActor {
  @_dynamicReplacement(for:_remote_helloAsyncThrows())
  nonisolated func _remote_impl_helloAsyncThrows() async throws -> String {
    "remote(\(#function))"
  }

  @_dynamicReplacement(for:_remote_helloAsync())
  nonisolated func _remote_impl_helloAsync() async throws -> String {
    "remote(\(#function))"
  }

  @_dynamicReplacement(for:_remote_helloThrows())
  nonisolated func _remote_impl_helloThrows() async throws -> String {
    "remote(\(#function))"
  }

  @_dynamicReplacement(for:_remote_hello())
  nonisolated func _remote_impl_hello() async throws -> String {
    "remote(\(#function))"
  }

  // === errors

  @_dynamicReplacement(for:_remote_helloThrowsImplBoom())
  nonisolated func _remote_impl_helloThrowsImplBoom() async throws -> String {
    "remote(\(#function))"
  }

  @_dynamicReplacement(for:_remote_helloThrowsTransportBoom())
  nonisolated func _remote_impl_helloThrowsTransportBoom() async throws -> String {
    throw Boom("transport")
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
    fatalError("not implemented:\(#function)")
  }

  func resolve<Act>(_ identity: Act.ID, as actorType: Act.Type)
  throws -> ActorResolved<Act>
      where Act: DistributedActor {
    .makeProxy
  }

  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    .init(ActorAddress(parse: ""))
  }

  func actorReady<Act>(_ actor: Act) where Act: DistributedActor {
  }

  func resignIdentity(_ id: AnyActorIdentity) {
  }
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test_remote_invoke(address: ActorAddress, transport: ActorTransport) async {
  func check(actor: SomeSpecificDistributedActor) async {
    let personality = __isRemoteActor(actor) ? "remote" : "local"

    let h1 = try! await actor.helloAsyncThrows()
    print("\(personality) - helloAsyncThrows: \(h1)")

    let h2 = try! await actor.helloAsync()
    print("\(personality) - helloAsync: \(h2)")

    let h3 = try! await actor.helloThrows()
    print("\(personality) - helloThrows: \(h3)")

    let h4 = try! await actor.hello()
    print("\(personality) - hello: \(h4)")

    // error throws
    if __isRemoteActor(actor) {
      do {
        _ = try await actor.helloThrowsTransportBoom()
        preconditionFailure("helloThrowsTransportBoom: should have thrown")
      } catch {
        print("\(personality) - helloThrowsTransportBoom: \(error)")
      }
    } else {
      do {
        _ = try await actor.helloThrowsImplBoom()
        preconditionFailure("helloThrowsImplBoom: Should have thrown")
      } catch {
        print("\(personality) - helloThrowsImplBoom: \(error)")
      }
    }
  }

  let remote = try! SomeSpecificDistributedActor(resolve: .init(address), using: transport)
  assert(__isRemoteActor(remote) == true, "should be remote")

  let local = SomeSpecificDistributedActor(transport: transport)
  assert(__isRemoteActor(local) == false, "should be local")

  print("local isRemote: \(__isRemoteActor(local))")
  // CHECK: local isRemote: false
  await check(actor: local)
  // CHECK: local - helloAsyncThrows: local(helloAsyncThrows())
  // CHECK: local - helloAsync: local(helloAsync())
  // CHECK: local - helloThrows: local(helloThrows())
  // CHECK: local - hello: local(hello())
  // CHECK: local - helloThrowsImplBoom: Boom(whoFailed: "impl")

  print("remote isRemote: \(__isRemoteActor(remote))")
  // CHECK: remote isRemote: true
  await check(actor: remote)
  // CHECK: remote - helloAsyncThrows: remote(_remote_impl_helloAsyncThrows())
  // CHECK: remote - helloAsync: remote(_remote_impl_helloAsync())
  // CHECK: remote - helloThrows: remote(_remote_impl_helloThrows())
  // CHECK: remote - hello: remote(_remote_impl_hello())
  // CHECK: remote - helloThrowsTransportBoom: Boom(whoFailed: "transport")

  print(local)
  print(remote)
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    let address = ActorAddress(parse: "")
    let transport = FakeTransport()

    await test_remote_invoke(address: address, transport: transport)
  }
}
