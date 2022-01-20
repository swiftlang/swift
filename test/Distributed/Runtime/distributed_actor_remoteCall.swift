// XXX: %target-swift-frontend -primary-file %s -emit-sil -parse-as-library -enable-experimental-distributed -disable-availability-checking | %FileCheck %s --enable-var-scope --dump-input=always
// RUN: %target-run-simple-swift( -Xfrontend -module-name=main -Xfrontend -disable-availability-checking -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://87568630 - segmentation fault on 32-bit WatchOS simulator
// UNSUPPORTED: OS=watchos && CPU=i386

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: windows

// FIXME(distributed): remote calls seem to hang on linux - rdar://87240034
// UNSUPPORTED: linux

import _Distributed

final class Obj: @unchecked Sendable, Codable  {}

struct LargeStruct: Sendable, Codable {
  var q: String
  var a: Int
  var b: Int64
  var c: Double
  var d: String
}

enum E : Sendable, Codable {
  case foo, bar
}

@_silgen_name("swift_distributed_actor_is_remote")
func __isRemoteActor(_ actor: AnyObject) -> Bool

distributed actor Greeter {
  distributed func empty() {
  }

  distributed func hello() -> String {
    return "Hello, World!"
  }

  distributed func answer() -> Int {
    return 42
  }

  distributed func largeResult() -> LargeStruct {
    .init(q: "question", a: 42, b: 1, c: 2.0, d: "Lorum ipsum")
  }

  distributed func echo(name: String, age: Int) -> String {
    return "Echo: name: \(name), age: \(age)"
  }

  distributed func enumResult() -> E {
    .bar
  }

}


// ==== Fake Transport ---------------------------------------------------------
struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address: String) {
    self.address = address
  }
}

struct FakeActorSystem: DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocation
  typealias InvocationEncoder = FakeInvocation
  typealias SerializationRequirement = Codable

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    let id = ActorAddress(parse: "xxx")
    return id
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
  }

  func resignID(_ id: ActorID) {
  }

  func makeInvocationEncoder() -> InvocationDecoder {
    .init()
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocationDecoder: InvocationDecoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
    Act.ID == ActorID,
    Res: SerializationRequirement {
    fatalError("INVOKED REMOTE CALL")
  }

}

struct FakeInvocation: DistributedTargetInvocationEncoder, DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  var substitutions: [Any.Type] = []
  var arguments: [Any] = []
  var returnType: Any.Type? = nil
  var errorType: Any.Type? = nil

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    substitutions.append(type)
  }
  mutating func recordArgument<Argument: SerializationRequirement>(_ argument: Argument) throws {
    arguments.append(argument)
  }
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
    self.errorType = type
  }
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
    self.returnType = type
  }
  mutating func doneRecording() throws {}

  // === Receiving / decoding -------------------------------------------------

  func decodeGenericSubstitutions() throws -> [Any.Type] {
    return substitutions
  }

  var argumentIndex: Int = 0
  mutating func decodeNextArgument<Argument>(
    _ argumentType: Argument.Type,
    into pointer: UnsafeMutablePointer<Argument>
  ) throws {
    guard argumentIndex < arguments.count else {
      fatalError("Attempted to decode more arguments than stored! Index: \(argumentIndex), args: \(arguments)")
    }

    let anyArgument = arguments[argumentIndex]
    guard let argument = anyArgument as? Argument else {
      fatalError("Cannot cast argument\(anyArgument) to expected \(Argument.self)")
    }

    print("  > decode argument: \(argument)")
    pointer.initialize(to: argument)
    argumentIndex += 1
  }

  func decodeErrorType() throws -> Any.Type? {
    self.errorType
  }

  func decodeReturnType() throws -> Any.Type? {
    self.returnType
  }
}

@available(SwiftStdlib 5.5, *)
struct FakeResultHandler: DistributedTargetInvocationResultHandler {
  typealias SerializationRequirement = Codable

  func onReturn<Res>(value: Res) async throws {
    print("RETURN: \(value)")
  }
  func onThrow<Err: Error>(error: Err) async throws {
    print("ERROR: \(error)")
  }
}

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// actual mangled name:
let emptyName = "$s4main7GreeterC5emptyyyFTE"
let helloName = "$s4main7GreeterC5helloSSyFTE"
let answerName = "$s4main7GreeterC6answerSiyFTE"
let largeResultName = "$s4main7GreeterC11largeResultAA11LargeStructVyFTE"
let enumResultName = "$s4main7GreeterC10enumResultAA1EOyFTE"
let echoName = "$s4main7GreeterC4echo4name3ageS2S_SitFTE"

func test() async throws {
  let system = FakeActorSystem()

  let local = Greeter(system: system)

  // act as if we decoded an Invocation:
  var emptyInvocation = FakeInvocation()

  try await system.executeDistributedTarget(
      on: local,
      mangledTargetName: emptyName,
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: ()

  try await system.executeDistributedTarget(
      on: local,
      mangledTargetName: helloName,
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: Hello, World!

  try await system.executeDistributedTarget(
      on: local,
      mangledTargetName: answerName,
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: 42

  try await system.executeDistributedTarget(
      on: local,
      mangledTargetName: largeResultName,
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: LargeStruct(q: "question", a: 42, b: 1, c: 2.0, d: "Lorum ipsum")

  try await system.executeDistributedTarget(
      on: local,
      mangledTargetName: enumResultName,
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: bar

  var echoInvocation = system.makeInvocationEncoder()
  try echoInvocation.recordArgument("Caplin")
  try echoInvocation.recordArgument(42)
  try echoInvocation.doneRecording()
  try await system.executeDistributedTarget(
      on: local,
      mangledTargetName: echoName,
      invocationDecoder: &echoInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: Echo: name: Caplin, age: 42

  print("done")
  // CHECK-NEXT: done
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
