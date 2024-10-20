// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -target %target-swift-5.7-abi-triple -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -target %target-swift-5.7-abi-triple -typecheck -verify -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

distributed actor YesVeryMuchSo {
  typealias ActorSystem = FakeRoundtripActorSystem
}
func test_YesVeryMuchSo(_ actor: YesVeryMuchSo) {
  let _: any Codable = actor // implicit conformance, ID was Codable

  // unrelated protocol
  let _: any CustomSerializationProtocol = actor // expected-error{{value of type 'YesVeryMuchSo' does not conform to specified type 'CustomSerializationProtocol'}}
}

// ==== ------------------------------------------------------------------------

distributed actor SerReqNotCodable {
  typealias ActorSystem = FakeCustomSerializationRoundtripActorSystem
}
func test_NotAtAll_NoImplicit(_ actor: SerReqNotCodable) {
  let _: any Codable = actor // OK, the ID was Codable, even though SerializationRequirement was something else

  // no implicit conformance
  let _: any CustomSerializationProtocol = actor // expected-error{{value of type 'SerReqNotCodable' does not conform to specified type 'CustomSerializationProtocol'}}
}

// ==== ------------------------------------------------------------------------

distributed actor NotAtAll_NothingCodable {
  typealias ActorSystem = FakeIdIsNotCodableActorSystem
}
func test_NotAtAll_NoImplicit(_ actor: NotAtAll_NothingCodable) {
  let _: any Codable = actor // expected-error{{value of type 'NotAtAll_NothingCodable' does not conform to specified type 'Decodable'}}

  // no implicit conformance
  let _: any CustomSerializationProtocol = actor // expected-error{{value of type 'NotAtAll_NothingCodable' does not conform to specified type 'CustomSerializationProtocol'}}
}

public struct NotCodableID: Sendable, Hashable {}

@available(SwiftStdlib 5.7, *)
public struct FakeIdIsNotCodableActorSystem: DistributedActorSystem, CustomStringConvertible {
  public typealias ActorID = NotCodableID
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeRoundtripResultHandler

  // just so that the struct does not become "trivial"
  let someValue: String = ""
  let someValue2: String = ""
  let someValue3: String = ""
  let someValue4: String = ""

  public init() {
  }

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
    where Act: DistributedActor,
    Act.ID == ActorID  {
    nil
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor,
    Act.ID == ActorID {
    .init()
  }

  public func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
  }

  public func resignID(_ id: ActorID) {
  }

  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  public func remoteCall<Act, Err, Res>(
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
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  public nonisolated var description: Swift.String {
    "\(Self.self)()"
  }
}
