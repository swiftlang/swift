// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

import _Concurrency

distributed actor LocalWorker {
}

// ==== Fake Transport ---------------------------------------------------------

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
let address = ActorAddress(parse: "")
let transport = FakeTransport()

func test() {
  _ = LocalWorker(transport: transport)
  // CHECK: assign type:LocalWorker, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.LocalWorker, address:[[ADDRESS]]
}

@main struct Main {
  static func main() async {
    test()
  }
}
