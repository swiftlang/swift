// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed


import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor DA {
}

@available(SwiftStdlib 5.5, *)
distributed actor DA_userDefined {
  deinit {}
}

@available(SwiftStdlib 5.5, *)
distributed actor DA_userDefined2 {
  deinit {
    print("Deinitializing \(self.actorAddress)")
    return
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
    print("resign address:\(address)")
  }
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test() {
  let transport = FakeTransport()

  _ = DA(transport: transport)
  // CHECK: assign type:DA, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA, address:[[ADDRESS]]
  // CHECK: resign address:[[ADDRESS]]

  _ = DA_userDefined(transport: transport)
  // CHECK: assign type:DA_userDefined, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_userDefined, address:[[ADDRESS]]
  // CHECK: resign address:[[ADDRESS]]

  // resign must happen as the _last thing_ after user-deinit completed
  _ = DA_userDefined2(transport: transport)
  // CHECK: assign type:DA_userDefined2, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_userDefined2, address:[[ADDRESS]]
  // CHECK: Deinitializing [[ADDRESS]]
  // CHECK-NEXT: resign address:[[ADDRESS]]
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    test()
  }
}
