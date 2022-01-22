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

struct S<T: Codable> : Codable {
  var data: T
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

  distributed func generic1<A: Codable>(a: A) {
    print("---> A = \(a), type(of:) = \(type(of: a))")
  }

  distributed func generic2<A: Codable, B: Codable>(a: A, b: B) {
    print("---> A = \(a), type(of:) = \(type(of: a))")
    print("---> B = \(b), type(of:) = \(type(of: b))")
  }

  distributed func generic3<A: Codable, B: Codable, C: Codable>(a: A, b: Array<B>, c: C) {
    print("---> A = \(a), type(of:) = \(type(of: a))")
    print("---> B = \(b), type(of:) = \(type(of: b))")
    print("---> C = \(c), type(of:) = \(type(of: c))")
  }

  distributed func generic4<A: Codable, B: Codable, C: Codable>(a: A, b: S<B>, c: Array<C>) {
    print("---> A = \(a), type(of:) = \(type(of: a))")
    print("---> B = \(b), type(of:) = \(type(of: b))")
    print("---> C = \(c), type(of:) = \(type(of: c))")
  }

  distributed func generic5<A: Codable, B: Codable, C: Codable, D: Codable>(a: A, b: S<B>, c: C, d: D) {
    print("---> A = \(a), type(of:) = \(type(of: a))")
    print("---> B = \(b), type(of:) = \(type(of: b))")
    print("---> C = \(c), type(of:) = \(type(of: c))")
    print("---> D = \(d), type(of:) = \(type(of: d))")
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
let generic1Name = "$s4main7GreeterC8generic11ayx_tSeRzSERzlFTE"
let generic2Name = "$s4main7GreeterC8generic21a1byx_q_tSeRzSERzSeR_SER_r0_lFTE"
let generic3Name = "$s4main7GreeterC8generic31a1b1cyx_Sayq_Gq0_tSeRzSERzSeR_SER_SeR0_SER0_r1_lFTE"
let generic4Name = "$s4main7GreeterC8generic41a1b1cyx_AA1SVyq_GSayq0_GtSeRzSERzSeR_SER_SeR0_SER0_r1_lFTE"
let generic5Name = "$s4main7GreeterC8generic51a1b1c1dyx_AA1SVyq_Gq0_q1_tSeRzSERzSeR_SER_SeR0_SER0_SeR1_SER1_r2_lFTE"

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

  var generic1Invocation = system.makeInvocationEncoder()

  try generic1Invocation.recordGenericSubstitution(Int.self)
  try generic1Invocation.recordArgument(42)
  try generic1Invocation.doneRecording()

  try await system.executeDistributedTarget(
    on: local,
    mangledTargetName: generic1Name,
    invocationDecoder: &generic1Invocation,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: RETURN: ()

  var generic2Invocation = system.makeInvocationEncoder()

  try generic2Invocation.recordGenericSubstitution(Int.self)
  try generic2Invocation.recordGenericSubstitution(String.self)
  try generic2Invocation.recordArgument(42)
  try generic2Invocation.recordArgument("Ultimate Question!")
  try generic2Invocation.doneRecording()

  try await system.executeDistributedTarget(
    on: local,
    mangledTargetName: generic2Name,
    invocationDecoder: &generic2Invocation,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: ---> B = Ultimate Question!, type(of:) = String
  // CHECK-NEXT: RETURN: ()

  var generic3Invocation = system.makeInvocationEncoder()

  try generic3Invocation.recordGenericSubstitution(Int.self)
  try generic3Invocation.recordGenericSubstitution(String.self)
  try generic3Invocation.recordGenericSubstitution(S<Int>.self)
  try generic3Invocation.recordArgument(42)
  try generic3Invocation.recordArgument(["a", "b", "c"])
  try generic3Invocation.recordArgument(S(data: 42))
  try generic3Invocation.doneRecording()

  try await system.executeDistributedTarget(
    on: local,
    mangledTargetName: generic3Name,
    invocationDecoder: &generic3Invocation,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: ---> B = ["a", "b", "c"], type(of:) = Array<String>
  // CHECK-NEXT: ---> C = S<Int>(data: 42), type(of:) = S<Int>
  // CHECK-NEXT: RETURN: ()

  var generic4Invocation = system.makeInvocationEncoder()

  try generic4Invocation.recordGenericSubstitution(Int.self)
  try generic4Invocation.recordGenericSubstitution(Int.self)
  try generic4Invocation.recordGenericSubstitution(String.self)
  try generic4Invocation.recordArgument(42)
  try generic4Invocation.recordArgument(S(data: 42))
  try generic4Invocation.recordArgument(["a", "b", "c"])
  try generic4Invocation.doneRecording()

  try await system.executeDistributedTarget(
    on: local,
    mangledTargetName: generic4Name,
    invocationDecoder: &generic4Invocation,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: ---> B = S<Int>(data: 42), type(of:) = S<Int>
  // CHECK-NEXT: ---> C = ["a", "b", "c"], type(of:) = Array<String>
  // CHECK-NEXT: RETURN: ()

  var generic5Invocation = system.makeInvocationEncoder()

  try generic5Invocation.recordGenericSubstitution(Int.self)
  try generic5Invocation.recordGenericSubstitution(Int.self)
  try generic5Invocation.recordGenericSubstitution(String.self)
  try generic5Invocation.recordGenericSubstitution([Double].self)
  try generic5Invocation.recordArgument(42)
  try generic5Invocation.recordArgument(S(data: 42))
  try generic5Invocation.recordArgument("Hello, World!")
  try generic5Invocation.recordArgument([0.0, 0xdecafbad])
  try generic5Invocation.doneRecording()

  try await system.executeDistributedTarget(
    on: local,
    mangledTargetName: generic5Name,
    invocationDecoder: &generic5Invocation,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: ---> B = S<Int>(data: 42), type(of:) = S<Int>
  // CHECK-NEXT: ---> C = Hello, World!, type(of:) = String
  // CHECK-NEXT: ---> D = [0.0, 3737844653.0], type(of:) = Array<Double>
  // CHECK-NEXT: RETURN: ()

  print("done")
  // CHECK-NEXT: done
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
