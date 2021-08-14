// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: rdar78290608

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor LocalWorker {
  distributed func function() async throws -> String {
    "local:"
  }

  distributed func echo(name: String) async throws -> String {
    "local:\(name)"
  }
}

@available(SwiftStdlib 5.5, *)
extension LocalWorker {
  @_dynamicReplacement(for: _remote_function())
  // TODO: @_remoteDynamicReplacement(for: function()) - could be a nicer spelling, hiding that we use dynamic under the covers
  func _cluster_remote_function() async throws -> String {
    "\(#function):"
  }

  @_dynamicReplacement(for: _remote_echo(name:))
  // TODO: @_remoteDynamicReplacement(for: hello(name:)) - could be a nicer spelling, hiding that we use dynamic under the covers
  func _cluster_remote_echo(name: String) async throws -> String {
    "\(#function):\(name)"
  }
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
    let id = ActorAddress(parse: "xxx")
    print("assign type:\(actorType), id:\(id)")
    return .init(id)
  }

  func actorReady<Act>(_ actor: Act) where Act: DistributedActor {
    print("ready actor:\(actor), id:\(actor.id)")
  }

  func resignIdentity(_ id: AnyActorIdentity) {
    print("ready id:\(id)")
  }
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test_local() async throws {
  let transport = FakeTransport()

  let worker = LocalWorker(transport: transport)
  let x = try await worker.function()
  print("call: \(x)")
  // CHECK: assign type:LocalWorker, id:[[ADDRESS:.*]]
  // CHECK: ready actor:main.LocalWorker, id:AnyActorIdentity([[ADDRESS]])
  // CHECK: call: local:
}

@available(SwiftStdlib 5.5, *)
func test_remote() async throws {
  let address = ActorAddress(parse: "")
  let transport = FakeTransport()

  let worker = try LocalWorker(resolve: .init(address), using: transport)
  let x = try await worker.function()
  print("call: \(x)")
  // CHECK: call: _cluster_remote_function():

  let e = try await worker.echo(name: "Charlie")
  print("call: \(e)")
  // CHECK: call: _cluster_remote_echo(name:):Charlie
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    try! await test_local()
    try! await test_remote()
  }
}
