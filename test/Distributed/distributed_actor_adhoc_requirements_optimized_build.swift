// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-6.0-abi-triple -emit-ir -swift-version 6 -O -I %t %s
// RUN: %target-swift-frontend -target %target-swift-6.0-abi-triple -emit-sil -swift-version 6 -O -I %t %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

// NOTE: None of the ad-hoc protocol requirement implementations

public protocol Transferable: Sendable {}

// NOT final on purpose
public class TheSpecificResultHandlerWhichIsANonFinalClass: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Transferable

  public func onReturn<Success>(value: Success) async throws where Success: Transferable {
  }

  public func onReturnVoid() async throws {
    fatalError()
  }

  public func onThrow<Err>(error: Err) async throws where Err : Error {
    fatalError()
  }
}

// NOT final on purpose
public class FakeInvocationDecoder: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Transferable

  public func decodeGenericSubstitutions() throws -> [Any.Type] {
    []
  }

  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    fatalError()
  }

  public func decodeErrorType() throws -> Any.Type? {
    nil
  }

  public func decodeReturnType() throws -> Any.Type? {
    nil
  }
}

// NOT final on purpose
public class FakeInvocationEncoder : DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Transferable

  public func recordArgument<Value: SerializationRequirement>(
    _ argument: RemoteCallArgument<Value>) throws {
  }

  public func recordGenericSubstitution<T>(_ type: T.Type) throws {
  }

  public func recordErrorType<E: Error>(_ type: E.Type) throws {
  }

  public func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
  }

  public func doneRecording() throws {
  }
}

// NOT final on purpose
public class NotFinalActorSystemForAdHocRequirementTest: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = String
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias SerializationRequirement = Transferable
  public typealias ResultHandler = TheSpecificResultHandlerWhichIsANonFinalClass

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    fatalError()
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    fatalError()
  }

  public func actorReady<Act>(_ actor: Act) where Act: DistributedActor, Act.ID == ActorID {
    fatalError()
  }

  public func resignID(_ id: ActorID) {
    fatalError()
  }

  public func makeInvocationEncoder() -> InvocationEncoder {
    fatalError()
  }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type,
    returning returnType: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    fatalError()
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type
  ) async throws
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error {
    fatalError()
  }
}

// FIXME: This call should be devirtualized but it cannot be done at the moment due to issues with ad-hoc serialization requirement.

// CHECK-LABEL: sil shared [transparent] [thunk] @$s52distributed_actor_adhoc_requirements_optimized_build42NotFinalActorSystemForAdHocRequirementTestC11Distributed0piJ0AadEP10remoteCall2on6target10invocation8throwing9returningqd_1_qd___AD06RemoteR6TargetV17InvocationEncoderQzzqd_0_mqd_1_mtYaKAD0pI0Rd__s5ErrorRd_0_2IDQyd__0I2IDRtzr1_lFTW
// CHECK: bb0(%0 : $*τ_0_2, %1 : $τ_0_0, %2 : $*RemoteCallTarget, %3 : $*FakeInvocationEncoder, %4 : $@thick τ_0_1.Type, %5 : $@thick τ_0_2.Type, %6 : $*NotFinalActorSystemForAdHocRequirementTest):
// CHECK-NEXT:  [[DIST_IMPL:%.*]] = load %6
// CHECK-NEXT:  [[REMOTE_CALL_WITNESS:%.*]] = class_method [[DIST_IMPL]] : $NotFinalActorSystemForAdHocRequirementTest, #NotFinalActorSystemForAdHocRequirementTest.remoteCall
// CHECK-NEXT: try_apply [[REMOTE_CALL_WITNESS]]<τ_0_0, τ_0_1, τ_0_2>(%0, %1, %2, %3, %4, %5, [[DIST_IMPL]])
