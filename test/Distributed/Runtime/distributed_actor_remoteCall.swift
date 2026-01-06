// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeCodableForDistributedTests.swiftmodule -module-name FakeCodableForDistributedTests -target %target-swift-5.7-abi-triple %S/../Inputs/FakeCodableForDistributedTests.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeCodableForDistributedTests.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed

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

distributed actor Greeter {
  distributed func echo(name: String, age: Int) -> String {
    return "Echo: name: \(name), age: \(age)"
  }

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

    // try encoding generic arguments to make sure that witness tables are passed correctly
    let json = FakeEncoder()

    let jsonA = try! json.encode(a)
    let jsonB = try! json.encode(b)
    let jsonC = try! json.encode(c)
    let jsonD = try! json.encode(d)

    print("---> A(SER) = \(jsonA)")
    print("---> B(SER) = \(jsonB)")
    print("---> C(SER) = \(jsonC)")
    print("---> D(SER) = \(jsonD)")
  }

  distributed func genericOptional<T: Codable>(t: T?) {
    print("---> T = \(t!), type(of:) = \(type(of: t))")
  }

  distributed func expectsDecodeError(v: Int???) {
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
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeResultHandler

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

  func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    fatalError("INVOKED REMOTE CALL")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error {
    fatalError("not implemented: \(#function)")
  }
}

struct FakeInvocationEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Codable

  var substitutions: [Any.Type] = []
  var arguments: [Any] = []
  var returnType: Any.Type? = nil
  var errorType: Any.Type? = nil

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    substitutions.append(type)
  }
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {
    arguments.append(argument.value)
  }
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
    self.errorType = type
  }
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
    self.returnType = type
  }
  mutating func doneRecording() throws {}

  // For testing only
  func makeDecoder() -> FakeInvocationDecoder {
    return .init(
      args: arguments,
      substitutions: substitutions,
      returnType: returnType,
      errorType: errorType
    )
  }
}


class FakeInvocationDecoder : DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  var arguments: [Any] = []
  var substitutions: [Any.Type] = []
  var returnType: Any.Type? = nil
  var errorType: Any.Type? = nil

  init(
    args: [Any],
    substitutions: [Any.Type] = [],
    returnType: Any.Type? = nil,
    errorType: Any.Type? = nil
  ) {
    self.arguments = args
    self.substitutions = substitutions
    self.returnType = returnType
    self.errorType = errorType
  }

  // === Receiving / decoding -------------------------------------------------
  func decodeGenericSubstitutions() throws -> [Any.Type] {
    return substitutions
  }

  var argumentIndex: Int = 0
  func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    guard argumentIndex < arguments.count else {
      fatalError("Attempted to decode more arguments than stored! Index: \(argumentIndex), args: \(arguments)")
    }

    let anyArgument = arguments[argumentIndex]
    guard let argument = anyArgument as? Argument else {
      fatalError("Cannot cast argument\(anyArgument) to expected \(Argument.self)")
    }

    if (argumentIndex == 0 && Argument.self == Int???.self) {
      throw ExecuteDistributedTargetError(message: "Failed to decode of Int??? (for a test)")
    }

    argumentIndex += 1
    return argument
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

  func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    print("RETURN: \(value)")
  }
  func onReturnVoid() async throws {
    print("RETURN VOID: ()")
  }
  func onThrow<Err: Error>(error: Err) async throws {
    print("ERROR: \(error)")
  }
}

// ==== ------------------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// actual mangled name:
let emptyName = "$s4main7GreeterC5emptyyyYaKFTE"
let helloName = "$s4main7GreeterC5helloSSyYaKFTE"
let answerName = "$s4main7GreeterC6answerSiyYaKFTE"
let largeResultName = "$s4main7GreeterC11largeResultAA11LargeStructVyYaKFTE"
let enumResultName = "$s4main7GreeterC10enumResultAA1EOyYaKFTE"
let echoName = "$s4main7GreeterC4echo4name3ageS2S_SitYaKFTE"
let generic1Name = "$s4main7GreeterC8generic11ayx_tYaKSeRzSERzlFTE"
let generic2Name = "$s4main7GreeterC8generic21a1byx_q_tYaKSeRzSERzSeR_SER_r0_lFTE"
let generic3Name = "$s4main7GreeterC8generic31a1b1cyx_Sayq_Gq0_tYaKSeRzSERzSeR_SER_SeR0_SER0_r1_lFTE"
let generic4Name = "$s4main7GreeterC8generic41a1b1cyx_AA1SVyq_GSayq0_GtYaKSeRzSERzSeR_SER_SeR0_SER0_r1_lFTE"
let generic5Name = "$s4main7GreeterC8generic51a1b1c1dyx_AA1SVyq_Gq0_q1_tYaKSeRzSERzSeR_SER_SeR0_SER0_SeR1_SER1_r2_lFTE"
let genericOptionalName = "$s4main7GreeterC15genericOptional1tyxSg_tYaKSeRzSERzlFTE"
let expectsDecodeErrorName = "$s4main7GreeterC18expectsDecodeError1vySiSgSgSg_tYaKFTE"

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(actorSystem: system)

  // act as if we decoded an Invocation:
  var emptyInvocation = FakeInvocationDecoder(args: [])

  try await system.executeDistributedTarget(
      on: local,
      target: RemoteCallTarget(emptyName),
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN VOID: ()

  try await system.executeDistributedTarget(
      on: local,
      target: RemoteCallTarget(helloName),
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: Hello, World!

  try await system.executeDistributedTarget(
      on: local,
      target: RemoteCallTarget(answerName),
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: 42

  try await system.executeDistributedTarget(
      on: local,
      target: RemoteCallTarget(largeResultName),
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: LargeStruct(q: "question", a: 42, b: 1, c: 2.0, d: "Lorum ipsum")

  try await system.executeDistributedTarget(
      on: local,
      target: RemoteCallTarget(enumResultName),
      invocationDecoder: &emptyInvocation,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: bar

  var echoInvocation = system.makeInvocationEncoder()
  try echoInvocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: "Caplin"))
  try echoInvocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: 42))
  try echoInvocation.doneRecording()

  var echoDecoder = echoInvocation.makeDecoder()
  try await system.executeDistributedTarget(
      on: local,
      target: RemoteCallTarget(echoName),
      invocationDecoder: &echoDecoder,
      handler: FakeResultHandler()
  )
  // CHECK: RETURN: Echo: name: Caplin, age: 42

  var generic1Invocation = system.makeInvocationEncoder()

  try generic1Invocation.recordGenericSubstitution(Int.self)
  try generic1Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: 42))
  try generic1Invocation.doneRecording()

  var generic1Decoder = generic1Invocation.makeDecoder()
  try await system.executeDistributedTarget(
    on: local,
    target: RemoteCallTarget(generic1Name),
    invocationDecoder: &generic1Decoder,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: RETURN VOID: ()

  var generic2Invocation = system.makeInvocationEncoder()

  try generic2Invocation.recordGenericSubstitution(Int.self)
  try generic2Invocation.recordGenericSubstitution(String.self)
  try generic2Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: 42))
  try generic2Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: "Ultimate Question!"))
  try generic2Invocation.doneRecording()

  var generic2Decoder = generic2Invocation.makeDecoder()
  try await system.executeDistributedTarget(
    on: local,
    target: RemoteCallTarget(generic2Name),
    invocationDecoder: &generic2Decoder,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: ---> B = Ultimate Question!, type(of:) = String
  // CHECK-NEXT: RETURN VOID: ()

  var generic3Invocation = system.makeInvocationEncoder()

  try generic3Invocation.recordGenericSubstitution(Int.self)
  try generic3Invocation.recordGenericSubstitution(String.self)
  try generic3Invocation.recordGenericSubstitution(S<Int>.self)
  try generic3Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: 42))
  try generic3Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: ["a", "b", "c"]))
  try generic3Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: S(data: 42)))
  try generic3Invocation.doneRecording()

  var generic3Decoder = generic3Invocation.makeDecoder()
  try await system.executeDistributedTarget(
    on: local,
    target: RemoteCallTarget(generic3Name),
    invocationDecoder: &generic3Decoder,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: ---> B = ["a", "b", "c"], type(of:) = Array<String>
  // CHECK-NEXT: ---> C = S<Int>(data: 42), type(of:) = S<Int>
  // CHECK-NEXT: RETURN VOID: ()

  var generic4Invocation = system.makeInvocationEncoder()

  try generic4Invocation.recordGenericSubstitution(Int.self)
  try generic4Invocation.recordGenericSubstitution(Int.self)
  try generic4Invocation.recordGenericSubstitution(String.self)
  try generic4Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: 42))
  try generic4Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: S(data: 42)))
  try generic4Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: ["a", "b", "c"]))
  try generic4Invocation.doneRecording()

  var generic4Decoder = generic4Invocation.makeDecoder()
  try await system.executeDistributedTarget(
    on: local,
    target: RemoteCallTarget(generic4Name),
    invocationDecoder: &generic4Decoder,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: ---> B = S<Int>(data: 42), type(of:) = S<Int>
  // CHECK-NEXT: ---> C = ["a", "b", "c"], type(of:) = Array<String>
  // CHECK-NEXT: RETURN VOID: ()

  var generic5Invocation = system.makeInvocationEncoder()

  try generic5Invocation.recordGenericSubstitution(Int.self)
  try generic5Invocation.recordGenericSubstitution(Int.self)
  try generic5Invocation.recordGenericSubstitution(String.self)
  try generic5Invocation.recordGenericSubstitution([Int].self)
  try generic5Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: 42))
  try generic5Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: S(data: 42)))
  try generic5Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: "Hello, World!"))
  try generic5Invocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: [0, 42]))
  try generic5Invocation.doneRecording()

  var generic5Decoder = generic5Invocation.makeDecoder()
  try await system.executeDistributedTarget(
    on: local,
    target: RemoteCallTarget(generic5Name),
    invocationDecoder: &generic5Decoder,
    handler: FakeResultHandler()
  )
  // CHECK: ---> A = 42, type(of:) = Int
  // CHECK-NEXT: ---> B = S<Int>(data: 42), type(of:) = S<Int>
  // CHECK-NEXT: ---> C = Hello, World!, type(of:) = String
  // CHECK-NEXT: ---> D = [0, 42], type(of:) = Array<Int>
  // CHECK-NEXT: ---> A(SER) = 42;
  // CHECK-NEXT: ---> B(SER) = data: 42;
  // CHECK-NEXT: ---> C(SER) = Hello, World!;
  // CHECK-NEXT: ---> D(SER) = 0: 0; 1: 42;
  // CHECK-NEXT: RETURN VOID: ()

  var genericOptInvocation = system.makeInvocationEncoder()

  try genericOptInvocation.recordGenericSubstitution([Int].self)
  try genericOptInvocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: [0, 42]))
  try genericOptInvocation.doneRecording()

  var genericOptDecoder = genericOptInvocation.makeDecoder()
  try await system.executeDistributedTarget(
    on: local,
    target: RemoteCallTarget(genericOptionalName),
    invocationDecoder: &genericOptDecoder,
    handler: FakeResultHandler()
  )
  // CHECK: ---> T = [0, 42], type(of:) = Optional<Array<Int>>
  // CHECK-NEXT: RETURN VOID: ()

  var decodeErrInvocation = system.makeInvocationEncoder()

  try decodeErrInvocation.recordArgument(RemoteCallArgument(label: "argument-name", name: "argument-name", value: 42))
  try decodeErrInvocation.doneRecording()

  var decodeErrDecoder = decodeErrInvocation.makeDecoder()
  try await system.executeDistributedTarget(
    on: local,
    target: RemoteCallTarget(expectsDecodeErrorName),
    invocationDecoder: &decodeErrDecoder,
    handler: FakeResultHandler()
  )
  // CHECK: ERROR: ExecuteDistributedTargetError(errorCode: Distributed.ExecuteDistributedTargetError.ErrorCode.other, message: "Failed to decode of Int??? (for a test)")

  print("done")
  // CHECK-NEXT: done
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
