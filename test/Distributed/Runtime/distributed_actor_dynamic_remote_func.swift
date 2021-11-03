// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): remote functions dont seem to work on windows?
// XFAIL: OS=windows-msvc

import _Distributed

distributed actor LocalWorker {
  init(transport: ActorTransport) {}

  distributed func function() async throws -> String {
    "local:"
  }

  distributed func echo(name: String) async throws -> String {
    "local:\(name)"
  }
}

extension LocalWorker {
  @_dynamicReplacement(for: _remote_function())
  // TODO(distributed): @_remoteDynamicReplacement(for: function()) - could be a nicer spelling, hiding that we use dynamic under the covers
  func _cluster_remote_function() async throws -> String {
    "\(#function):"
  }

  @_dynamicReplacement(for: _remote_echo(name:))
  // TODO(distributed): @_remoteDynamicReplacement(for: hello(name:)) - could be a nicer spelling, hiding that we use dynamic under the covers
  func _cluster_remote_echo(name: String) async throws -> String {
    "\(#function):\(name)"
  }
}

// ==== Fake Transport ---------------------------------------------------------

struct ActorAddress: ActorIdentity {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

struct FakeTransport: ActorTransport {
  func decodeIdentity(from decoder: Decoder) throws -> AnyActorIdentity {
    fatalError("not implemented:\(#function)")
  }

  func resolve<Act>(_ identity: AnyActorIdentity, as actorType: Act.Type)
  throws -> Act?
      where Act: DistributedActor {
    return nil
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

func test_local() async throws {
  let transport = FakeTransport()

  let worker = LocalWorker(transport: transport)
  let x = try await worker.function()
  print("call: \(x)")
  // CHECK: assign type:LocalWorker, id:[[ADDRESS:.*]]
  // CHECK: ready actor:main.LocalWorker, id:AnyActorIdentity([[ADDRESS]])
  // CHECK: call: local:
}

func test_remote() async throws {
  let address = ActorAddress(parse: "")
  let transport = FakeTransport()

  let worker = try LocalWorker.resolve(.init(address), using: transport)
  let x = try await worker.function()
  print("call: \(x)")
  // CHECK: call: _cluster_remote_function():

  let e = try await worker.echo(name: "Charlie")
  print("call: \(e)")
  // CHECK: call: _cluster_remote_echo(name:):Charlie
}

@main struct Main {
  static func main() async {
    try! await test_local()
    try! await test_remote()
  }
}
