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
import _Concurrency

struct Boom: Error {}

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
   throw Boom()
 }

 distributed func helloThrowsTransportBoom() throws -> String {
   "local(\(#function))"
 }

}

@available(SwiftStdlib 5.5, *)
extension SomeSpecificDistributedActor {

  static func _remote_helloAsyncThrows(actor: SomeSpecificDistributedActor) async throws -> String {
    return "remote(\(#function)) (address: \(actor.actorAddress))"
  }

  static func _remote_helloAsync(actor: SomeSpecificDistributedActor) async throws -> String {
    return "remote(\(#function)) (address: \(actor.actorAddress))"
  }

  static func _remote_helloThrows(actor: SomeSpecificDistributedActor) async throws -> String {
    return "remote(\(#function)) (address: \(actor.actorAddress))"
  }

  static func _remote_hello(actor: SomeSpecificDistributedActor) async throws -> String {
    return "remote(\(#function)) (address: \(actor.actorAddress))"
  }

  // === errors

  static func _remote_helloThrowsImplBoom(actor: SomeSpecificDistributedActor) async throws -> String {
    throw Boom()
  }

  static func _remote_helloThrowsTransportBoom(actor: SomeSpecificDistributedActor) async throws -> String {
    throw Boom()
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
struct FakeTransport: ActorTransport {
  func resolve<Act>(address: ActorAddress, as actorType: Act.Type)
    throws -> ActorResolved<Act> where Act: DistributedActor {
    return .makeProxy
  }

  func assignAddress<Act>(
    _ actorType: Act.Type
  ) -> ActorAddress where Act : DistributedActor {
    ActorAddress(parse: "")
  }

  public func actorReady<Act>(
    _ actor: Act
  ) where Act: DistributedActor {}

  public func resignAddress(
    _ address: ActorAddress
  ) {}
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
        preconditionFailure("Should have thrown")
      } catch {
        print("\(personality) - helloThrowsTransportBoom: \(error)")
      }

      do {
        _ = try await actor.helloThrowsImplBoom()
        preconditionFailure("Should have thrown")
      } catch {
        print("\(personality) - helloThrowsImplBoom: \(error)")
      }
    }
  }

  let remote = try! SomeSpecificDistributedActor(resolve: address, using: transport)
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


  print("remote isRemote: \(__isRemoteActor(remote))")
  // CHECK: remote isRemote: true
  await check(actor: remote)
  // CHECK: remote - helloAsyncThrows: remote(_remote_helloAsyncThrows(actor:))
  // CHECK: remote - helloAsync: remote(_remote_helloAsync(actor:))
  // CHECK: remote - helloThrows: remote(_remote_helloThrows(actor:))
  // CHECK: remote - hello: remote(_remote_hello(actor:))
  // CHECK: remote - helloThrowsTransportBoom: Boom()
  // CHECK: remote - helloThrowsImplBoom: Boom()

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
