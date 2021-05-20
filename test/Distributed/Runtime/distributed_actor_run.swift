// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
distributed actor SomeSpecificDistributedActor {

  distributed func hello() async throws {
     print("hello from \(self.actorAddress)")
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension SomeSpecificDistributedActor {
  static func _remote_hello(actor: SomeSpecificDistributedActor) async throws {
    print("Remote invocation")
  }
}

// ==== Execute ----------------------------------------------------------------

@_silgen_name("swift_distributed_actor_is_remote")
func __isRemoteActor(_ actor: AnyObject) -> Bool

func __isLocalActor(_ actor: AnyObject) -> Bool {
  return !__isRemoteActor(actor)
}


// ==== Fake Transport ---------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
struct FakeTransport: ActorTransport {
  func resolve<Act>(address: ActorAddress, as actorType: Act.Type)
    throws -> ActorResolved<Act> where Act: DistributedActor {
    fatalError()
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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_initializers() {
  let address = ActorAddress(parse: "")
  let transport = FakeTransport()

  _ = SomeSpecificDistributedActor(transport: transport)
  _ = try! SomeSpecificDistributedActor(resolve: address, using: transport)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_address() {
  let transport = FakeTransport()

  let actor = SomeSpecificDistributedActor(transport: transport)
  _ = actor.actorAddress
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_run(transport: FakeTransport) async {
  let actor = SomeSpecificDistributedActor(transport: transport)

  print("before") // CHECK: before
  try! await actor.hello()
  print("after") // CHECK: after
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await test_run(transport: FakeTransport())
  }
}
