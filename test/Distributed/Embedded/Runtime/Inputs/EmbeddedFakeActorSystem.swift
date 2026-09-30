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

import _Concurrency
import Distributed

// ==== ----------------------------------------------------------------------
// MARK: The serialization requirement
//
// We cannot use Codable in Embedded Swift so actor systems most likely "bring their own favorite coding mechanism".
// We simulate one here which might be a typical choice, a requested size and then encoding into a preallocated buffer
// offered as an OutputSpan. This is just a toy encoder/decoder, real impls could do something else.

public protocol EmbeddedSerializationRequirement {
  // The exact number of bytes `encode(into:)` will append.
  var serializedByteCount: Int { get }

  // Append exactly `serializedByteCount` bytes describing this value
  func encode(into output: inout OutputSpan<UInt8>)

  // Reconstruct a value by consuming bytes off the front of `input`,
  // advancing it past what was read
  static func decode(from input: inout Span<UInt8>) throws -> Self
}

// ==== ----------------------------------------------------------------------
// MARK: A tiny length-prefixed wire format
//
// This is a trivial lenght-prefixed format: "<decimal-byte-count>|<payload>"

private let sep = UInt8(ascii: "|")

// Render an `Int` as its decimal ASCII bytes, without going through `String`.
// Public so other modules can serialize integer fields of their own types
public func asciiDigits(_ value: Int) -> [UInt8] {
  if value == 0 { return [UInt8(ascii: "0")] }
  var v = value
  let negative = v < 0
  var digits: [UInt8] = []
  // Build least-significant digit first, then reverse
  while v != 0 {
    let d = v % 10
    // Works for negative v too: -(d) is a single digit 0...9
    digits.append(UInt8(ascii: "0") + UInt8(d < 0 ? -d : d))
    v /= 10
  }
  if negative { digits.append(UInt8(ascii: "-")) }
  return Array(digits.reversed())
}

// Parse decimal ASCII bytes back into an `Int`, without going through `String`.
// Public so other modules can deserialize integer fields of their own types
public func parseInt(_ bytes: ArraySlice<UInt8>) -> Int? {
  if bytes.isEmpty { return nil }
  var result = 0
  var negative = false
  var idx = bytes.startIndex
  if bytes[idx] == UInt8(ascii: "-") {
    negative = true
    idx = bytes.index(after: idx)
    if idx == bytes.endIndex { return nil }
  }
  while idx != bytes.endIndex {
    let b = bytes[idx]
    guard b >= UInt8(ascii: "0"), b <= UInt8(ascii: "9") else { return nil }
    result = result * 10 + Int(b - UInt8(ascii: "0"))
    idx = bytes.index(after: idx)
  }
  return negative ? -result : result
}

// Copy all remaining bytes of a borrowed `Span` into an owned array and advance
// the span past them. Public so other modules can implement `decode(from:)` for
// their own types without touching `Span`'s element API directly
public func drain(_ input: inout Span<UInt8>) -> [UInt8] {
  let n = input.count
  var out: [UInt8] = []
  out.reserveCapacity(n)
  var i = 0
  while i < n {
    out.append(input[i])
    i += 1
  }
  input = input.extracting(droppingFirst: n)
  return out
}

// ==== ----------------------------------------------------------------------
// MARK: Serialization conformances for the built-in payload types

extension String: EmbeddedSerializationRequirement {
  public var serializedByteCount: Int { utf8.count }
  public func encode(into output: inout OutputSpan<UInt8>) {
    for byte in utf8 { output.append(byte) }
  }
  public static func decode(from input: inout Span<UInt8>) throws -> String {
    String(decoding: drain(&input), as: UTF8.self)
  }
}

extension Int: EmbeddedSerializationRequirement {
  public var serializedByteCount: Int { asciiDigits(self).count }
  public func encode(into output: inout OutputSpan<UInt8>) {
    for byte in asciiDigits(self) { output.append(byte) }
  }
  public static func decode(from input: inout Span<UInt8>) throws -> Int {
    let bytes = drain(&input)
    guard let n = parseInt(bytes[...]) else { throw WireError.badValue }
    return n
  }
}

final class CallBuffer {
  var argBytes: [UInt8] = []
  init() {}
}

final class ResultBuffer {
  var returnBytes: [UInt8] = []
  init() {}
}

public enum WireError: Error {
  case underflow
  case malformed
  case badValue
}

// ==== ----------------------------------------------------------------------
// MARK: Encoder / Decoder / ResultHandler

public struct EmbeddedFakeInvocationEncoder: DistributedTargetInvocationEncoder {
  let buffer: CallBuffer
  init(buffer: CallBuffer) { self.buffer = buffer }
  public mutating func doneRecording() throws {}
}
extension EmbeddedFakeInvocationEncoder {
  // The single generic record method. The synthesized distributed thunk emits a
  // specialized call to it for each argument; there are no per-type overloads
  public mutating func recordArgument<Value: EmbeddedSerializationRequirement>(
      _ argument: RemoteCallArgument<Value>) throws {
    appendField(argument.value)
  }

  // Append one length-prefixed field to the request wire: the payload's byte
  // count as decimal ASCII, a "|" separator, then exactly that many payload
  // bytes written straight into freshly reserved array capacity through an
  // `OutputSpan`. Generic over the serialization protocol, so any conforming
  // type serializes the same way
  public func appendField<Value: EmbeddedSerializationRequirement>(_ value: Value) {
    let n = value.serializedByteCount
    buffer.argBytes.append(contentsOf: asciiDigits(n))
    buffer.argBytes.append(sep)
    buffer.argBytes.append(addingCapacity: n) { output in
      value.encode(into: &output)
    }
  }
}

public struct EmbeddedFakeInvocationDecoder: DistributedTargetInvocationDecoder {
  let buffer: CallBuffer
  // Read cursor into `buffer.argBytes`; each `decodeNextArgument` advances it
  var offset: Int = 0
  init(buffer: CallBuffer) { self.buffer = buffer }
}

extension EmbeddedFakeInvocationDecoder {
  public mutating func decodeNextArgument<Argument: EmbeddedSerializationRequirement>() throws -> Argument {
    let payload = try takeField()
    let owned = Array(payload)
    var span = owned.span
    return try Argument.decode(from: &span)
  }

  // Peel one length-prefixed field off the front of the incoming wire,
  // returning its payload bytes and advancing the read cursor past them. Public
  // so other modules' `decode(from:)` conformances can reuse the framing
  public mutating func takeField() throws -> ArraySlice<UInt8> {
    let bytes = buffer.argBytes
    guard offset < bytes.count else { throw WireError.underflow }
    var i = offset
    while i < bytes.count, bytes[i] != sep { i += 1 }
    guard i < bytes.count else { throw WireError.malformed }
    guard let n = parseInt(bytes[offset..<i]), n >= 0 else { throw WireError.malformed }
    let start = i + 1
    let end = start + n
    guard end <= bytes.count else { throw WireError.underflow }
    offset = end
    return bytes[start..<end]
  }
}

public struct EmbeddedFakeResultHandler: DistributedTargetInvocationResultHandler {
  let buffer: ResultBuffer
  init(buffer: ResultBuffer) { self.buffer = buffer }
  public func onReturnVoid() async throws { buffer.returnBytes = [] }
  public func onThrow(error: any Error) async throws {
    fatalError("threw in handler")
  }

  // Serialize the single response field, length-prefixed like the request
  // fields. Symmetric with the encoder's `appendField`
  public func writeResponse<Value: EmbeddedSerializationRequirement>(_ value: Value) {
    let n = value.serializedByteCount
    var out: [UInt8] = []
    out.append(contentsOf: asciiDigits(n))
    out.append(sep)
    out.append(addingCapacity: n) { output in
      value.encode(into: &output)
    }
    buffer.returnBytes = out
  }
}
extension EmbeddedFakeResultHandler {
  // The single generic result method. The synthesized receive-dispatch emits a
  // specialized call to it with the distributed func's return value
  public func onReturn<Success: EmbeddedSerializationRequirement>(value: Success) async throws {
    writeResponse(value)
  }
}

// ==== ----------------------------------------------------------------------
// MARK: The actor system

public struct EmbeddedFakeActorID: Sendable, Hashable {
  public let id: UInt64
  public init(id: UInt64) { self.id = id }
}

public final class EmbeddedFakeRoundtripActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = EmbeddedFakeActorID
  public typealias SerializationRequirement = EmbeddedSerializationRequirement
  public typealias InvocationEncoder = EmbeddedFakeInvocationEncoder
  public typealias InvocationDecoder = EmbeddedFakeInvocationDecoder
  public typealias ResultHandler = EmbeddedFakeResultHandler

  // Each hosted actor's monomorphized receive entrypoint.
  public typealias LocalDispatch =
    (borrowing RemoteCallTarget, inout InvocationDecoder, ResultHandler) async throws -> Void

  var active: [ActorID: LocalDispatch] = [:]
  var nextID: UInt64 = 1

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ActorSystem == EmbeddedFakeRoundtripActorSystem {
    return nil // always remote: calls route through `remoteCall`
  }
  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ActorSystem == EmbeddedFakeRoundtripActorSystem {
    defer { nextID += 1 }
    return ActorID(id: nextID)
  }

  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ActorSystem == EmbeddedFakeRoundtripActorSystem {
    // This specific pattern is necessary so that the transparent executeDistributedTarget
    // gets inlined here and works on a concrete Act then, otherwise it would not work in Embedded.
    active[actor.id] = { [self] target, decoder, handler in
      try await self.executeDistributedTarget(
          on: actor, target: target, invocationDecoder: &decoder, handler: handler)
    }
  }
  public func resignID(_ id: ActorID) {
    active.removeValue(forKey: id)
  }

  public func makeInvocationEncoder() -> InvocationEncoder { .init(buffer: CallBuffer()) }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor,
            Act.ID == ActorID,
            Err: Error,
            Res: EmbeddedSerializationRequirement {
    print("[swift] remoteCall reached")
    guard let dispatch = active[actor.id] else {
      fatalError("no local actor hosted for the target id")
    }

    // The caller's encoder holds the fully serialized arguments. Read them out
    // as the request payload and hand a copy to the callee - the array value
    // copy is the "network": the callee gets its own buffer, not a shared ref
    let requestWire = invocation.buffer.argBytes

    // ==================== NETWORK: request -> callee =====================
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = requestWire
    var decoder = InvocationDecoder(buffer: requestBuffer)

    // The callee decodes its arguments from `decoder`, runs the target, and
    // serializes its return value into this response buffer
    let resultBuffer = ResultBuffer()
    let handler = ResultHandler(buffer: resultBuffer)
    try await dispatch(target, &decoder, handler)

    // Take the serialized response back across the wire and decode it into `Res`
    let responseWire = resultBuffer.returnBytes

    // ==================== NETWORK: response -> caller ====================
    let responseBuffer = CallBuffer()
    responseBuffer.argBytes = responseWire
    var responseDecoder = InvocationDecoder(buffer: responseBuffer)
    return try responseDecoder.decodeNextArgument()
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    print("[swift] remoteCallVoid reached")
    guard let dispatch = active[actor.id] else {
      fatalError("no local actor hosted for the target id")
    }

    // Same request crossing as `remoteCall`: hand the callee a copy of the
    // serialized arguments over the wire
    let requestWire = invocation.buffer.argBytes
    // ==================== NETWORK: request -> callee =====================
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = requestWire
    var decoder = InvocationDecoder(buffer: requestBuffer)

    // The callee decodes its arguments and runs the target. A void target
    // resolves through the handler's `onReturnVoid`, which writes an empty
    // response - there is nothing to read back
    let resultBuffer = ResultBuffer()
    let handler = ResultHandler(buffer: resultBuffer)
    try await dispatch(target, &decoder, handler)
  }
}
