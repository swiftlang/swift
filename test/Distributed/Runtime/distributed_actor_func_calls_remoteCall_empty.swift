// XXX: %target-swift-frontend -primary-file %s -emit-sil -parse-as-library -enable-experimental-distributed -disable-availability-checking | %FileCheck %s --enable-var-scope --dump-input=always
// RUN: %target-run-simple-swift( -Xfrontend -module-name=main -Xfrontend -disable-availability-checking -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: windows

// FIXME(distributed): remote calls seem to hang on linux - rdar://87240034
// UNSUPPORTED: linux

import _Distributed

distributed actor Greeter {
  distributed func empty() {
  }
}


// ==== Fake Transport ---------------------------------------------------------
struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address: String) {
    self.address = address
  }
}

//final class FakeActorSystem: DistributedActorSystem {
struct FakeActorSystem: DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocation
  typealias InvocationEncoder = FakeInvocation
  typealias SerializationRequirement = Codable

  init() {}

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
    invocationDecoder: inout InvocationDecoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Err: Error,
//          Act.ID == ActorID,
          Res: SerializationRequirement {
    print("remoteCall: on:\(actor), target:\(target), invocation:\(invocationDecoder), throwing:\(throwing), returning:\(returning)")
    return () as! Res
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocationDecoder: inout InvocationDecoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
          Err: Error
//          Act.ID == ActorID
  {
    print("remoteCallVoid: on:\(actor), target:\(target), invocation:\(invocationDecoder), throwing:\(throwing)")
    return ()
  }

}

struct FakeInvocation: DistributedTargetInvocationEncoder, DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  var arguments: [Any] = []
  var returnType: Any.Type? = nil
  var errorType: Any.Type? = nil

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    fatalError("NOT IMPLEMENTED: \(#function)")
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
    []
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
    pointer.pointee = argument
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

let echoName = "$s4main7GreeterC4echo4nameS2S_tFTE"

func test() async throws {
  let system = FakeActorSystem()

  let local = Greeter(system: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  try await ref.empty()
  // CHECK: remoteCallVoid: on:main.Greeter, target:RemoteCallTarget(_mangledName: "$s4main7GreeterC5emptyyyFTE"), invocation:FakeInvocation(arguments: [], returnType: nil, errorType: nil, argumentIndex: 0), throwing:Never
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
