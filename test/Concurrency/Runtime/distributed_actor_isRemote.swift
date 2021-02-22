// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

import Dispatch
import _Concurrency

distributed actor SomeSpecificDistributedActor {
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

func test_remote() async {
  let local = SomeSpecificDistributedActor(transport: transport)
  _ = local.actorAddress
  assert(__isLocalActor(local) == true, "should be local")
  assert(__isRemoteActor(local) == false, "should be local")

  // assume it always makes a remote one
//  let remote = SomeSpecificDistributedActor(resolve: address, using: transport)
//  assert(__isLocalActor(remote) == false, "should be remote")
//  assert(__isRemoteActor(remote) == true, "should be remote")

  print("done") // CHECK: done
}

@main struct Main {
  static func main() async {
    await test_remote()
  }
}
