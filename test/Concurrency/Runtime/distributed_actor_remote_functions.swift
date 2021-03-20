// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency

import Dispatch
import _Concurrency

struct Boom: Error {}

distributed actor SomeSpecificDistributedActor {
  let state: String = "hi there"

  distributed func helloAsyncThrows() async throws -> String {
    "local(\(#function))"
  }

//  distributed func helloAsync() async -> String {
//    "local(\(#function))"
//  }
//
//  distributed func helloThrows() throws -> String {
//    "local(\(#function))"
//  }
//
//  distributed func hello() -> String {
//    "local(\(#function))"
//  }
//
//  // === errors
//
//  distributed func helloThrowsImplBoom() throws -> String {
//    throw Boom()
//  }
//
//  distributed func helloThrowsTransportBoom() throws -> String {
//    "local(\(#function))"
//  }

}

extension SomeSpecificDistributedActor {

  static func _remote_helloAsyncThrows(actor: SomeSpecificDistributedActor) async throws -> String {
    return "remote(\(#function)) (address: \(actor.actorAddress))"
  }

//  static func _remote_helloAsync(actor: SomeSpecificDistributedActor) async throws -> String {
//    return "remote(\(#function)) (address: \(actor.actorAddress))"
//  }
//
//  static func _remote_helloThrows(actor: SomeSpecificDistributedActor) async throws -> String {
//    return "remote(\(#function)) (address: \(actor.actorAddress))"
//  }
//
//  static func _remote_hello(actor: SomeSpecificDistributedActor) async throws -> String {
//    return "remote(\(#function)) (address: \(actor.actorAddress))"
//  }
//
//  // === errors
//
//  static func _remote_helloThrowsImplBoom(actor: SomeSpecificDistributedActor) async throws -> String {
//    return "remote(\(#function)) (address: \(actor.actorAddress))"
//  }
//
//  static func _remote_helloThrowsTransportBoom(actor: SomeSpecificDistributedActor) async throws -> String {
//    throw Boom()
//  }
}

// ==== Fake Transport ---------------------------------------------------------

struct FakeTransport: ActorTransport {
  func resolve<Act>(address: ActorAddress, as actorType: Act.Type)
    throws -> ActorResolved<Act> where Act: DistributedActor {
    return .makeProxy
  }

  func assignAddress<Act>(
    _ actorType: Act.Type
//    ,
//    onActorCreated: (Act) -> ()
  ) -> ActorAddress where Act : DistributedActor {
    ActorAddress(parse: "")
  }
}

// ==== Execute ----------------------------------------------------------------
let address = ActorAddress(parse: "")
let transport = FakeTransport()

func test_remote_invoke() async {
  func check(actor: SomeSpecificDistributedActor) async {
    let personality = __isRemoteActor(actor) ? "remote" : "local"

    let h1 = try! await actor.helloAsyncThrows()
    print("\(personality) - helloAsyncThrows: \(h1)")

//    let h2 = try! await remote.helloAsync()
//    print("\(personality) - helloAsync: \(h2)")
//
//    let h3 = try! await remote.helloThrows()
//    print("\(personality) - helloThrows: \(h3)")
//
//    let h4 = try! await remote.hello()
//    print("\(personality) - hello: \(h4)")
//
//    // error throws
//    do {
//      try await remote.helloThrowsTransportBoom()
//      preconditionFailure("Should have thrown")
//    } catch {
//      print("\(personality) - helloThrowsTransportBoom: \(error)")
//    }
//
//    do {
//      try await remote.helloThrowsImplBoom()
//      preconditionFailure("Should have thrown")
//    } catch {
//      print("\(personality) - helloThrowsImplBoom: \(error)")
//    }
  }

  let remote = try! SomeSpecificDistributedActor(resolve: address, using: transport)
  assert(__isRemoteActor(remote) == true, "should be remote")

  let local = SomeSpecificDistributedActor(transport: transport)
  assert(__isRemoteActor(local) == false, "should be local")

  print("local isRemote: \(__isRemoteActor(local))")
  // CHECK: local isRemote: false
  await check(actor: local)
  // CHECK: local - helloAsyncThrows: local(helloAsyncThrows())


  print("remote isRemote: \(__isRemoteActor(remote))")
  // CHECK: remote isRemote: true
  await check(actor: remote)
  // CHECK: remote - helloAsyncThrows: remote(_remote_helloAsyncThrows(actor:))

  print(local)
  print(remote)
}

@main struct Main {
  static func main() async {
    await test_remote_invoke()
  }
}
