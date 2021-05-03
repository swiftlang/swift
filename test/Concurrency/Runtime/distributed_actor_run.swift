// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
distributed actor SomeSpecificDistributedActor {
//  // @derived let actorTransport: ActorTransport
//  // @derived let actorAddress: ActorAddress

//  // @derived
//  required init(transport: ActorTransport) {
//    self.actorTransport = transport
//    self.actorAddress = ActorAddress(parse: "xxx")
//  }
//  // @derived
//  required init(resolve address: ActorAddress, using transport: ActorTransport) {
//    self.actorAddress = address
//    self.actorTransport = transport
//  }

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
