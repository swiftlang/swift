// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: radar78290608

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor LocalWorker {
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
func test() {
  let transport = FakeTransport()

  _ = LocalWorker(transport: transport)
  // CHECK: assign type:LocalWorker, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.LocalWorker, address:[[ADDRESS]]
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    test()
  }
}
