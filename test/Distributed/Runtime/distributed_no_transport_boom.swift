// RUN: %target-fail-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library %import-libdispatch) 2>&1 | %FileCheck %s
//
// // TODO: could not figure out how to use 'not --crash' it never is used with target-run-simple-swift
// This test is intended to *crash*, so we're using target-fail-simple-swift
// which expects the exit code of the program to be non-zero;
// We then check stderr for the expected error message using filecheck as usual.

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// REQUIRES: radar78290608

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SomeSpecificDistributedActor {
  distributed func hello() async throws -> String {
    "local impl"
  }
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
func test_remote() async {
  let address = ActorAddress(parse: "")
  let transport = FakeTransport()

  let remote = try! SomeSpecificDistributedActor(resolve: address, using: transport)
  _ = try! await remote.hello() // let it crash!
  // CHECK: SOURCE_DIR/test/Distributed/Runtime/distributed_no_transport_boom.swift:16: Fatal error: Invoked remote placeholder function '_remote_hello' on remote distributed actor of type 'main.SomeSpecificDistributedActor'. Configure an appropriate 'ActorTransport' for this actor to resolve this error (e.g. by depending on some specific transport library).
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_remote()
  }
}

