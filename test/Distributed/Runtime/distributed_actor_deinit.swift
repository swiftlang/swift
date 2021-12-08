// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: windows

import _Distributed

@available(SwiftStdlib 5.6, *)
actor A {}

@available(SwiftStdlib 5.6, *)
distributed actor DA {
  init(transport: FakeActorSystem) {}
}

@available(SwiftStdlib 5.6, *)
distributed actor DA_userDefined {
  init(transport: FakeActorSystem) {}

  deinit {}
}

@available(SwiftStdlib 5.6, *)
distributed actor DA_userDefined2 {
  init(transport: FakeActorSystem) {}

  deinit {
    print("Deinitializing \(self.id)")
    return
  }
}

@available(SwiftStdlib 5.6, *)
distributed actor DA_state {
  var name = "Hello"
  var age = 42

  init(transport: FakeActorSystem) {}

  deinit {
    print("Deinitializing \(self.id)")
    return
  }
}

// ==== Fake Transport ---------------------------------------------------------

@available(SwiftStdlib 5.6, *)
struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

@available(SwiftStdlib 5.6, *)
final class FakeActorSystem: @unchecked Sendable, DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias Invocation = FakeDistributedInvocation

  var n = 0

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor,
            Act.ID == ActorID {
    print("resolve type:\(actorType), address:\(id)")
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor,
            Act.ID == ActorID {
    n += 1
    let address = ActorAddress(parse: "addr-\(n)")
    print("assign type:\(actorType), address:\(address)")
    return address
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
    print("ready actor:\(actor), address:\(actor.id)")
  }

  func resignID(_ id: ActorID) {
    print("resign address:\(id)")
  }

  @inlinable func makeInvocation() throws -> Invocation {
    .init()
  }
}

struct FakeDistributedInvocation: DistributedTargetInvocation {
  typealias  ArgumentDecoder = FakeDistributedTargetInvocationArgumentDecoder
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(mangledType: T.Type) throws { }
  mutating func recordArgument<Argument: SerializationRequirement>(argument: Argument) throws { }
  mutating func recordReturnType<R: SerializationRequirement>(mangledType: R.Type) throws { }
  mutating func recordErrorType<E: Error>(mangledType: E.Type) throws { }
  mutating func doneRecording() throws { }

  // === Receiving / decoding -------------------------------------------------

  mutating func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  mutating func argumentDecoder() -> Self.ArgumentDecoder { .init() }
  mutating func decodeReturnType() throws -> Any.Type? { nil }
  mutating func decodeErrorType() throws -> Any.Type? { nil }
}

struct FakeDistributedTargetInvocationArgumentDecoder: DistributedTargetInvocationArgumentDecoder {
  typealias SerializationRequirement = Codable
}

@available(SwiftStdlib 5.6, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.6, *)
func test() {
  let transport = FakeActorSystem()

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
  // CHECK: ready actor:main.DA, address:ActorAddress(address: "[[ADDR1:addr-[0-9]]]")
  // CHECK: resign address:ActorAddress(address: "[[ADDR1]]")

  _ = { () -> DA_userDefined in
    DA_userDefined(transport: transport)
  }()
  // CHECK: assign type:DA_userDefined, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_userDefined, address:ActorAddress(address: "[[ADDR2:addr-[0-9]]]")
  // CHECK: resign address:ActorAddress(address: "[[ADDR2]]")

  // resign must happen as the _last thing_ after user-deinit completed
  _ = { () -> DA_userDefined2 in
    DA_userDefined2(transport: transport)
  }()
  // CHECK: assign type:DA_userDefined2, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_userDefined2, address:ActorAddress(address: "[[ADDR3:addr-[0-9]]]")
  // CHECK: Deinitializing ActorAddress(address: "[[ADDR3]]")
  // CHECK-NEXT: resign address:ActorAddress(address: "[[ADDR3]]")

  // resign must happen as the _last thing_ after user-deinit completed
  _ = { () -> DA_state in
    DA_state(transport: transport)
  }()
  // CHECK: assign type:DA_state, address:[[ADDRESS:.*]]
  // CHECK: ready actor:main.DA_state, address:ActorAddress(address: "[[ADDR4:addr-[0-9]]]")
  // CHECK: Deinitializing ActorAddress(address: "[[ADDR4]]")
  // CHECK-NEXT: resign address:ActorAddress(address: "[[ADDR4]]")

  // a remote actor should not resign it's address, it was never "assigned" it
  let address = ActorAddress(parse: "remote-1")
  _ = { () -> DA_userDefined2 in
    try! DA_userDefined2.resolve(id: address, using: transport)
  }()
  // CHECK-NEXT: resolve type:DA_userDefined2, address:ActorAddress(address: "[[ADDR5:remote-1]]")
  // CHECK-NEXT: Deinitializing
}

@available(SwiftStdlib 5.6, *)
@main struct Main {
  static func main() async {
    test()
  }
}
