//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Distributed
import EmbeddedFakeActorSystem

@Resolvable
public protocol Greeter: DistributedActor where ActorSystem == EmbeddedFakeRoundtripActorSystem {
  distributed func hello(name: String) -> String
  distributed func farewell(name: String) -> String

  distributed func note(_ message: String)

  distributed func check(_ uid: ComplexRequest) -> ComplexResponse
}


// ==== ---------------------------------------------------------------------------
// API module can declare whole API surface, including "complex" types.

public struct ComplexRequest: Sendable {
  public let id: Int
  public init(id: Int) { self.id = id }
}
public struct ComplexResponse: Sendable {
  public let id: Int
  public init(id: Int) { self.id = id }
}

// ==== ---------------------------------------------------------------------------
// As long as we also provide serialization for it:

extension ComplexRequest: EmbeddedFakeRoundtripActorSystem.SerializationRequirement {
  public var serializedByteCount: Int { asciiDigits(id).count }
  public func encode(into output: inout OutputSpan<UInt8>) {
    for byte in asciiDigits(id) { output.append(byte) }
  }
  public static func decode(from input: inout Span<UInt8>) throws -> ComplexRequest {
    guard let id = parseInt(drain(&input)[...]) else { throw WireError.badValue }
    return ComplexRequest(id: id)
  }
}

extension ComplexResponse: EmbeddedSerializationRequirement {
  public var serializedByteCount: Int { asciiDigits(id).count }
  public func encode(into output: inout OutputSpan<UInt8>) {
    for byte in asciiDigits(id) { output.append(byte) }
  }
  public static func decode(from input: inout Span<UInt8>) throws -> ComplexResponse {
    guard let id = parseInt(drain(&input)[...]) else { throw WireError.badValue }
    return ComplexResponse(id: id)
  }
}
