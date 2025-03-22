// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-irgen -swift-version 6 -O -I %t %s

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

public protocol Serializable {
  func withUnsafeBytesSerialization<Result>(_ body: (UnsafeRawBufferPointer) throws -> Result) rethrows -> Result
}

public protocol Deserializable {
  init(fromSerializedBuffer buffer: UnsafeRawBufferPointer) throws
}

public protocol Transferable: Serializable & Deserializable & Sendable {}

public class RemoteCallResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Transferable

  public func onReturn<Success>(value: Success) async throws where Success: Transferable {
    fatalError()
  }

  public func onReturnVoid() async throws {
    fatalError()
  }

  public func onThrow<Err>(error: Err) async throws where Err : Error {
    fatalError()
  }


}
