// RUN: %target-run-simple-swift( -target %target-swift-5.7-abi-triple -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime


// FIXME(distributed): Seems something remains incorrect here
// REQUIRES: rdar92952551

import Distributed

enum MyError: Error {
  case test
}

distributed actor PickATransport1 {
  init(kappa system: FakeActorSystem, other: Int) {
    self.actorSystem = system
  }
}

distributed actor PickATransport2 {
  init(other: Int, theSystem: FakeActorSystem) async {
    self.actorSystem = theSystem
  }
}

distributed actor LocalWorker {
  init(system: FakeActorSystem) {
    self.actorSystem = system
  }
}

distributed actor Bug_CallsReadyTwice {
  var x: Int
  init(system: FakeActorSystem, wantBug: Bool) async {
    self.actorSystem = system
    if wantBug {
      self.x = 1
    }
    self.x = 2
  }
}

distributed actor Throwy {
  init(system: FakeActorSystem, doThrow: Bool) throws {
    self.actorSystem = system
    if doThrow {
      throw MyError.test
    }
  }
}

distributed actor ThrowBeforeFullyInit {
  var x: Int
  init(system: FakeActorSystem, doThrow: Bool) throws {
    self.actorSystem = system
    if doThrow {
      throw MyError.test
    }
    self.x = 0
  }
}

distributed actor ThrowingAssign {
  init(_ getSystem: @Sendable () throws -> FakeActorSystem) throws {
    self.actorSystem = try getSystem()
  }
}

distributed actor MaybeSystem {
  init?(_ sys: FakeActorSystem?) {
    if let system = sys {
      self.actorSystem = system
      return
    }
    return nil
  }
}

distributed actor MaybeAfterAssign {
  var x: Int
  init?(fail: Bool) {
    actorSystem = FakeActorSystem()
    if fail {
      return nil
    }
    x = 100
  }
}

distributed actor LocalTestingDA_Int {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  var int: Int
  init() {
    actorSystem = .init()
    int = 12
    // CRASH
  }
}

// ==== Fake Transport ---------------------------------------------------------

struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address: String) {
    self.address = address
  }
}

// global to track available IDs
var nextID: Int = 1

struct FakeActorSystem: DistributedActorSystem {
  public typealias ActorID = ActorAddress
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeResultHandler

  init() {
    print("Initialized new FakeActorSystem")
  }

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor,
            Act.ID == ActorID  {
    fatalError("not implemented:\(#function)")
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor {
    let id = ActorAddress(parse: "\(nextID)")
    nextID += 1
    print("assign type:\(actorType), id:\(id)")
    return id
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
    print("ready actor:\(actor), id:\(actor.id)")
  }

  func resignID(_ id: ActorID) {
    print("resign id:\(id)")
  }

  func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  func remoteCall<Act, Err, Res>(
      on actor: Act,
      target: RemoteCallTarget,
      invocation: inout InvocationEncoder,
      throwing: Err.Type,
      returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    throw ExecuteDistributedTargetError(message: "Not implemented")
  }

  func remoteCallVoid<Act, Err>(
      on actor: Act,
      target: RemoteCallTarget,
      invocation: inout InvocationEncoder,
      throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error {
    throw ExecuteDistributedTargetError(message: "Not implemented")
  }

}

// === Sending / encoding -------------------------------------------------
struct FakeInvocationEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

// === Receiving / decoding -------------------------------------------------
class FakeInvocationDecoder : DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  func decodeReturnType() throws -> Any.Type? { nil }
  func decodeErrorType() throws -> Any.Type? { nil }
}

public struct FakeResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Codable

  public func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    fatalError("Not implemented: \(#function)")
  }

  public func onReturnVoid() async throws {
    fatalError("Not implemented: \(#function)")
  }

  public func onThrow<Err: Error>(error: Err) async throws {
    fatalError("Not implemented: \(#function)")
  }
}

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

func test() async {
  let system = DefaultDistributedActorSystem()

  // NOTE: All allocated distributed actors should be saved in this array, so
  // that they will be deallocated together at the end of this test!
  // This convention helps ensure that the test is not flaky.
  var test: [(any DistributedActor)?] = []

  test.append(LocalWorker(system: system))
  // CHECK: assign type:LocalWorker, id:ActorAddress(address: "[[ID1:.*]]")
  // CHECK: ready actor:main.LocalWorker, id:ActorAddress(address: "[[ID1]]")

  test.append(PickATransport1(kappa: system, other: 0))
  // CHECK: assign type:PickATransport1, id:ActorAddress(address: "[[ID2:.*]]")
  // CHECK: ready actor:main.PickATransport1, id:ActorAddress(address: "[[ID2]]")

  test.append(try? Throwy(system: system, doThrow: false))
  // CHECK: assign type:Throwy, id:ActorAddress(address: "[[ID3:.*]]")
  // CHECK: ready actor:main.Throwy, id:ActorAddress(address: "[[ID3]]")

  test.append(try? Throwy(system: system, doThrow: true))
  // CHECK: assign type:Throwy, id:ActorAddress(address: "[[ID4:.*]]")
  // CHECK-NOT: ready
  // CHECK: resign id:ActorAddress(address: "[[ID4]]")

  test.append(try? ThrowBeforeFullyInit(system: system, doThrow: true))
  // CHECK: assign type:ThrowBeforeFullyInit, id:ActorAddress(address: "[[ID5:.*]]")
  // CHECK-NOT: ready
  // CHECK: resign id:ActorAddress(address: "[[ID5]]")

  test.append(await PickATransport2(other: 1, theSystem: system))
  // CHECK: assign type:PickATransport2, id:ActorAddress(address: "[[ID6:.*]]")
  // CHECK: ready actor:main.PickATransport2, id:ActorAddress(address: "[[ID6]]")

  test.append(await Bug_CallsReadyTwice(system: system, wantBug: true))
    // CHECK: assign type:Bug_CallsReadyTwice, id:ActorAddress(address: "[[ID7:.*]]")
    // CHECK:      ready actor:main.Bug_CallsReadyTwice, id:ActorAddress(address: "[[ID7]]")
    // CHECK-NEXT: ready actor:main.Bug_CallsReadyTwice, id:ActorAddress(address: "[[ID7]]")

  test.append(MaybeSystem(system))
  // CHECK: assign type:MaybeSystem, id:ActorAddress(address: "[[ID8:.*]]")
  // CHECK:      ready actor:main.MaybeSystem, id:ActorAddress(address: "[[ID8]]")

  test.append(MaybeAfterAssign(fail: true))
  // CHECK:      assign type:MaybeAfterAssign, id:ActorAddress(address: "[[ID9:.*]]")
  // CHECK-NOT:  ready
  // CHECK-NEXT: resign id:ActorAddress(address: "[[ID9]]")

  test.append(MaybeAfterAssign(fail: false))
  // CHECK:      assign type:MaybeAfterAssign, id:ActorAddress(address: "[[ID10:.*]]")
  // CHECK-NEXT: ready actor:main.MaybeAfterAssign, id:ActorAddress(address: "[[ID10]]")

  let localDA = LocalTestingDA_Int()
  print("localDA = \(localDA.id)")
  // CHECK: localDA = LocalTestingActorID(id: "1")

  // the following tests fail to initialize the actor's identity.
  print("-- start of no-assign tests --")
  test.append(MaybeSystem(nil))
  test.append(try? ThrowingAssign { throw MyError.test })
  print("-- end of no-assign tests --")
  // CHECK: -- start of no-assign tests --
  // CHECK-NOT: assign
  // CHECK: -- end of no-assign tests --


  // resigns that come out of the deinits:

  // CHECK-DAG: resign id:ActorAddress(address: "[[ID1]]")
  // CHECK-DAG: resign id:ActorAddress(address: "[[ID2]]")
  // CHECK-DAG: resign id:ActorAddress(address: "[[ID3]]")

  // CHECK-DAG: resign id:ActorAddress(address: "[[ID6]]")
  // CHECK-DAG: resign id:ActorAddress(address: "[[ID7]]")
  // CHECK-DAG: resign id:ActorAddress(address: "[[ID8]]")

  // CHECK-DAG: resign id:ActorAddress(address: "[[ID10]]")
}

@main struct Main {
  static func main() async {
    await test()
  }
}
