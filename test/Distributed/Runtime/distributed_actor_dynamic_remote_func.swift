// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

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
struct FakeTransport: ActorTransport {
  func resolve<Act>(address: ActorAddress, as actorType: Act.Type)
  throws -> ActorResolved<Act> where Act: DistributedActor {
    fatalError()
  }

  func assignAddress<Act>(
    _ actorType: Act.Type
  ) -> ActorAddress where Act : DistributedActor {
    let address = ActorAddress(parse: "xxx")
    print("assign type:\(actorType), address:\(address)")
    return address
  }

  public func actorReady<Act>(
    _ actor: Act
  ) where Act: DistributedActor {
    print("ready actor:\(actor), address:\(actor.actorAddress)")
  }

  public func resignAddress(
    _ address: ActorAddress
  ) {
    print("ready address:\(address)")
  }
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test_local() async throws {
  let transport = FakeTransport()

  let worker = LocalWorker(transport: transport)
  let x = try await worker.function()
  print("call: \(x)")
  // CHECK: assign type:LocalWorker, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.LocalWorker, address:[[ADDRESS]]
  // CHECK: call: local:
}

@available(SwiftStdlib 5.5, *)
func test_remote() async throws {
  let address = ActorAddress(parse: "")
  let transport = FakeTransport()

  let worker = try LocalWorker(resolve: address, using: transport)
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
