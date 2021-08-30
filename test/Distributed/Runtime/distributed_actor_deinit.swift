// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

@available(SwiftStdlib 5.5, *)
    actor A {}

@available(SwiftStdlib 5.5, *)
distributed actor DA {
  init(transport: ActorTransport) {
    defer { transport.actorReady(self) }
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor DA_userDefined {
  init(transport: ActorTransport) {
    defer { transport.actorReady(self) }
  }

  deinit {}
}

@available(SwiftStdlib 5.5, *)
distributed actor DA_userDefined2 {
  init(transport: ActorTransport) {
    defer { transport.actorReady(self) }
  }

  deinit {
    print("Deinitializing \(self.id)")
    return
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor DA_state {
  var name = "Hello"
  var age = 42

  init(transport: ActorTransport) {
    defer { transport.actorReady(self) }
  }

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
final class FakeTransport: @unchecked Sendable, ActorTransport {

  var n = 0

  func decodeIdentity(from decoder: Decoder) throws -> AnyActorIdentity {
    print("decode identity from:\(decoder)")
    fatalError("not implemented \(#function)")
  }

  func resolve<Act>(_ identity: AnyActorIdentity, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor {
    print("resolve type:\(actorType), address:\(identity)")
    return nil
  }

  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    n += 1
    let address = ActorAddress(parse: "addr-\(n)")
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

  // no lifecycle things make sense for a normal actor, double check we didn't emit them
  print("before A")
  _ = A()
  print("after A")
  // CHECK: before A
  // CHECK: after A

  _ = { () -> DA in
    DA(transport: transport)
  }()
  // CHECK: assign type:DA, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA, address:AnyActorIdentity(ActorAddress(address: "[[ADDR1:addr-[0-9]]]"))
  // CHECK: resign address:AnyActorIdentity(ActorAddress(address: "[[ADDR1]]"))

  _ = { () -> DA_userDefined in
    DA_userDefined(transport: transport)
  }()
  // CHECK: assign type:DA_userDefined, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_userDefined, address:AnyActorIdentity(ActorAddress(address: "[[ADDR2:addr-[0-9]]]"))
  // CHECK: resign address:AnyActorIdentity(ActorAddress(address: "[[ADDR2]]"))

  // resign must happen as the _last thing_ after user-deinit completed
  _ = { () -> DA_userDefined2 in
    DA_userDefined2(transport: transport)
  }()
  // CHECK: assign type:DA_userDefined2, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_userDefined2, address:AnyActorIdentity(ActorAddress(address: "[[ADDR3:addr-[0-9]]]"))
  // CHECK: Deinitializing AnyActorIdentity(ActorAddress(address: "[[ADDR3]]"))
  // CHECK-NEXT: resign address:AnyActorIdentity(ActorAddress(address: "[[ADDR3]]"))

  // resign must happen as the _last thing_ after user-deinit completed
  _ = { () -> DA_state in
    DA_state(transport: transport)
  }()
  // CHECK: assign type:DA_state, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_state, address:AnyActorIdentity(ActorAddress(address: "[[ADDR4:addr-[0-9]]]"))
  // CHECK: Deinitializing AnyActorIdentity(ActorAddress(address: "[[ADDR4]]"))
  // CHECK-NEXT: resign address:AnyActorIdentity(ActorAddress(address: "[[ADDR4]]"))

  // a remote actor should not resign it's address, it was never "assigned" it
  let address = ActorAddress(parse: "remote-1")
  _ = { () -> DA_userDefined2 in
    try! DA_userDefined2.resolve(.init(address), using: transport)
  }()
  // CHECK-NEXT: resolve type:DA_userDefined2, address:AnyActorIdentity(ActorAddress(address: "[[ADDR5:remote-1]]"))
  // CHECK-NEXT: Deinitializing
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    test()
  }
}
