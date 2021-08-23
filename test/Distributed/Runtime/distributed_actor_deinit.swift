// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: rdar78290608

import _Distributed

@available(SwiftStdlib 5.5, *)
actor A {}

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
    print("Deinitializing \(self.id)")
    return
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor DA_state {
  var name = "Hello"
  var age = 42

  deinit {
    print("Deinitializing \(self.id)")
    return
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
    print("decode identity from:\(decoder)")
    fatalError("not implemented \(#function)")
  }

  func resolve<Act>(_ identity: Act.ID, as actorType: Act.Type) throws -> ActorResolved<Act>
      where Act: DistributedActor {
    print("resolve type:\(actorType), address:\(identity)")
    return .makeProxy
  }

  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    let address = ActorAddress(parse: "xxx")
    print("assign type:\(actorType), address:\(address)")
    return .init(address)
  }

  public func actorReady<Act>(_ actor: Act) where Act: DistributedActor {
    print("ready actor:\(actor), address:\(actor.id)")
  }

  func resignIdentity(_ identity: AnyActorIdentity) {
    print("resign address:\(identity)")
  }
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test() {
  let transport = FakeTransport()
  let address = ActorAddress(parse: "xxx")

  // no lifecycle things make sense for a normal actor, double check we didn't emit them
  print("before A")
  _ = A()
  print("after A")
  // CHECK: before A
  // CHECK: after A

  _ = DA(transport: transport)
  // CHECK: assign type:DA, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA, address:AnyActorIdentity(ActorAddress(address: "xxx"))
  // CHECK: resign address:AnyActorIdentity(ActorAddress(address: "xxx"))

  _ = DA_userDefined(transport: transport)
  // CHECK: assign type:DA_userDefined, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_userDefined, address:AnyActorIdentity(ActorAddress(address: "xxx"))
  // CHECK: resign address:AnyActorIdentity(ActorAddress(address: "xxx"))

  // resign must happen as the _last thing_ after user-deinit completed
  _ = DA_userDefined2(transport: transport)
  // CHECK: assign type:DA_userDefined2, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_userDefined2, address:AnyActorIdentity(ActorAddress(address: "xxx"))
  // CHECK: Deinitializing AnyActorIdentity(ActorAddress(address: "xxx"))
  // CHECK-NEXT: resign address:AnyActorIdentity(ActorAddress(address: "xxx"))

  // resign must happen as the _last thing_ after user-deinit completed
  _ = DA_state(transport: transport)
  // CHECK: assign type:DA_state, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_state, address:AnyActorIdentity(ActorAddress(address: "xxx"))
  // CHECK: Deinitializing AnyActorIdentity(ActorAddress(address: "xxx"))
  // CHECK-NEXT: resign address:AnyActorIdentity(ActorAddress(address: "xxx"))

  // a remote actor should not resign it's address, it was never "assigned" it
  print("before")
  _ = try! DA_userDefined2(resolve: .init(address), using: transport)
  print("done")
  // CHECK: before
  // CHECK-NEXT: Deinitializing
  // CHECK-NEXT: done
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    test()
  }
}
