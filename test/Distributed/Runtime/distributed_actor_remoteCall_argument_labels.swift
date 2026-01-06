// RUN: %target-run-simple-swift( -target %target-swift-5.7-abi-triple -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: OS=windows-msvc

// The returned "effective" label changed in 5.9, to fix an incorrect behavior,
// so we skip the test in previous releases:
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed

@available(SwiftStdlib 5.5, *)
distributed actor ParamsDA {

  // CHECK: argument.label: Optional("a")
  // CHECK: argument.name: a
  // CHECK: argument.effectiveLabel: a
  distributed func callMe(a: String) {}

  // CHECK: argument.label: Optional("b")
  // CHECK: argument.name: c
  // CHECK: argument.effectiveLabel: b
  distributed func callMe(b c: String) {}

  // CHECK: argument.label: nil
  // CHECK: argument.name: d
  // CHECK: argument.effectiveLabel: _
  distributed func callMe(_ d: String) {}

  // CHECK: argument.label: nil
  // CHECK: argument.name: p0
  // CHECK: argument.effectiveLabel: _
  distributed func callMe2(_: String) {}
}

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

@main struct Main {
  static func main() async {
    let system = FakeActorSystem()
    let pda = try! ParamsDA.resolve(id: .init(parse: "x"), using: system)
    print("--- (a a)")
    try! await pda.callMe(a: "")
    print("--- (b c)")
    try! await pda.callMe(b: "")
    print("--- (_ d)")
    try! await pda.callMe(_: "")
    print("--- (_)")
    try! await pda.callMe2(_: "")
    print("OK")
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
    return // expected; mock out replying
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
    print("argument.label: \(String(describing: argument.label))")
    print("argument.name: \(argument.name)")
    print("argument.effectiveLabel: \(argument.effectiveLabel)")
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
