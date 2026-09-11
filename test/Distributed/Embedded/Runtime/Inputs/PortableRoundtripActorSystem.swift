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

// A reusable in-memory `DistributedActorSystem` that plays both ends of a call,
// forcing every argument and return value through a real serialized `[UInt8]`
// wire (never passed by shared reference or cast through `Any`).
//
// The SAME source compiles both with and without Embedded Swift; only the
// per-value (de)serialization and the `remoteCall` family shape differ,
// behind `#if $Embedded` below. Actor-agnostic, so any portable test can
// declare its own `distributed actor` against it.

import _Concurrency
import Distributed

// ==== ----------------------------------------------------------------------
// MARK: Shared byte-wire framing (identical in both modes)
//
// A value crosses as one length-prefixed field: "<decimal-byte-count>|<bytes>".
// The length prefix lets the decoder find each field boundary without a type
// tag, since the expected type is known statically at each call site. All of
// this is plain `[UInt8]` arithmetic - no `String` grapheme work - so it links
// in Embedded Swift without the Unicode data tables.

private let fieldSeparator = UInt8(ascii: "|")

// Render an `Int` as decimal ASCII bytes without going through `String`.
public func asciiDigits(_ value: Int) -> [UInt8] {
  if value == 0 { return [UInt8(ascii: "0")] }
  var v = value
  var digits: [UInt8] = []
  while v != 0 {
    digits.append(UInt8(ascii: "0") + UInt8(v % 10))
    v /= 10
  }
  return Array(digits.reversed())
}

// Parse decimal ASCII bytes back into an `Int` without going through `String`.
public func parseInt(_ bytes: ArraySlice<UInt8>) -> Int? {
  if bytes.isEmpty { return nil }
  var result = 0
  for b in bytes {
    guard b >= UInt8(ascii: "0"), b <= UInt8(ascii: "9") else { return nil }
    result = result * 10 + Int(b - UInt8(ascii: "0"))
  }
  return result
}

// Append one length-prefixed field to `wire`.
public func appendField(_ payload: [UInt8], to wire: inout [UInt8]) {
  wire.append(contentsOf: asciiDigits(payload.count))
  wire.append(fieldSeparator)
  wire.append(contentsOf: payload)
}

// Peel one length-prefixed field off `wire`, advancing `offset` past it.
public func takeField(_ wire: [UInt8], _ offset: inout Int) -> [UInt8]? {
  guard offset < wire.count else { return nil }
  var i = offset
  while i < wire.count, wire[i] != fieldSeparator { i += 1 }
  guard i < wire.count, let n = parseInt(wire[offset..<i]), n >= 0 else { return nil }
  let start = i + 1
  let end = start + n
  guard end <= wire.count else { return nil }
  offset = end
  return Array(wire[start..<end])
}

// The request / response payload. `remoteCall` copies the bytes out of one of
// these into a brand-new one for the far end - that copy is the "network".
public final class CallBuffer {
  public var argBytes: [UInt8] = []
  public var returnBytes: [UInt8] = []
  public init() {}
}

// UInt64-backed on purpose: a String-backed id would risk pulling the Unicode
// data tables into the embedded link. Ids never appear on the wire.
public struct PortableActorID: Sendable, Hashable {
  public let id: UInt64
  public init(id: UInt64) { self.id = id }
}

#if $Embedded

// ==== ----------------------------------------------------------------------
// MARK: Embedded per-value serialization

// Embedded can't use `Codable`; the system binds `SerializationRequirement` to
// this byte protocol and conforming types render to bytes through the naive JSON
// writer below. This system only moves `String`, so that is all that conforms.
public protocol PortableSerializationRequirement {
  func toWireBytes() -> [UInt8]
  // Decode takes the actor system so that distributed-actor references can be
  // reconstructed by resolving their id - the embedded analog of Codable's
  // `decoder.userInfo[.actorSystemKey]`. `throws` so `resolve` can propagate;
  // value types (like `String`) just ignore the system and never throw.
  static func fromWireBytes(_ bytes: [UInt8],
                            system: PortableRoundtripActorSystem) throws -> Self
}

// ==== ----------------------------------------------------------------------
// MARK: A naive JSON writer / reader
//
// The whole point of the embedded serialization layer is that it needs nothing
// from Foundation: a few dozen lines of `[UInt8]` appends produce JSON that is
// byte-for-byte what `JSONEncoder` emits for the shapes we use. A future
// macro-based "Codable v2" would slot in right here. Everything stays raw
// `[UInt8]` (no `String` grapheme / normalization work) so the embedded link
// does not pull in the Unicode data tables.

// Build a flat JSON object one field at a time: `appendField("value", string: x)`
// emits `"value":"x"` (quoted, minimally escaped); `appendField("n", number: 7)`
// emits `"n":7`. No space after the colon and keys in insertion order - exactly
// how `JSONEncoder` renders, so the bytes are interchangeable with the Codable
// side.
struct JSONWriter {
  private var bytes: [UInt8] = [UInt8(ascii: "{")]
  private var first = true

  private mutating func writeKey(_ key: StaticString) {
    if !first { bytes.append(UInt8(ascii: ",")) }
    first = false
    bytes.append(UInt8(ascii: "\""))
    var p = key.utf8Start
    let end = p + key.utf8CodeUnitCount
    while p < end { bytes.append(p.pointee); p += 1 }
    bytes.append(UInt8(ascii: "\""))
    bytes.append(UInt8(ascii: ":"))
  }

  mutating func appendField(_ key: StaticString, string value: String) {
    writeKey(key)
    bytes.append(UInt8(ascii: "\""))
    for b in value.utf8 {
      // Minimal JSON string escaping: backslash and double-quote
      if b == UInt8(ascii: "\"") || b == UInt8(ascii: "\\") {
        bytes.append(UInt8(ascii: "\\"))
      }
      bytes.append(b)
    }
    bytes.append(UInt8(ascii: "\""))
  }

  mutating func appendField(_ key: StaticString, number value: Int) {
    writeKey(key)
    bytes.append(contentsOf: asciiDigits(value))
  }

  mutating func finish() -> [UInt8] {
    bytes.append(UInt8(ascii: "}"))
    return bytes
  }
}

// Read a single field back out of a flat JSON object. Just enough to undo what
// `JSONWriter` wrote - a naive `"<key>":` scan followed by a quoted-string or
// decimal-number read. Not a general JSON parser
struct JSONReader {
  let bytes: [UInt8]
  init(_ bytes: [UInt8]) { self.bytes = bytes }

  // Byte offset just past `"<key>":`, or nil if the key is absent
  private func valueStart(after key: StaticString) -> Int? {
    var needle: [UInt8] = [UInt8(ascii: "\"")]
    var p = key.utf8Start
    let end = p + key.utf8CodeUnitCount
    while p < end { needle.append(p.pointee); p += 1 }
    needle.append(UInt8(ascii: "\""))
    needle.append(UInt8(ascii: ":"))
    if bytes.count < needle.count { return nil }
    var i = 0
    while i <= bytes.count - needle.count {
      var j = 0
      while j < needle.count, bytes[i + j] == needle[j] { j += 1 }
      if j == needle.count { return i + needle.count }
      i += 1
    }
    return nil
  }

  func string(field key: StaticString) -> String? {
    guard var i = valueStart(after: key), i < bytes.count,
          bytes[i] == UInt8(ascii: "\"") else { return nil }
    i += 1
    var out: [UInt8] = []
    while i < bytes.count {
      let b = bytes[i]
      if b == UInt8(ascii: "\\") {
        i += 1
        if i < bytes.count { out.append(bytes[i]) } // unescape: take next byte literally
      } else if b == UInt8(ascii: "\"") {
        return String(decoding: out, as: UTF8.self)
      } else {
        out.append(b)
      }
      i += 1
    }
    return nil
  }

  func number(field key: StaticString) -> Int? {
    guard let i = valueStart(after: key) else { return nil }
    var j = i
    while j < bytes.count {
      let b = bytes[j]
      if (b >= UInt8(ascii: "0") && b <= UInt8(ascii: "9")) || b == UInt8(ascii: "-") {
        j += 1
      } else {
        break
      }
    }
    return parseInt(bytes[i..<j])
  }
}

extension String: PortableSerializationRequirement {
  public func toWireBytes() -> [UInt8] {
    var w = JSONWriter()
    w.appendField("value", string: self)
    return w.finish() // {"value":"..."} - identical to the Codable Box<String> JSON
  }
  public static func fromWireBytes(_ bytes: [UInt8],
                                   system: PortableRoundtripActorSystem) -> String {
    let r = JSONReader(bytes)
    return r.string(field: "value") ?? ""
  }
}

public struct PortableEncoder: DistributedTargetInvocationEncoder {
  let buffer: CallBuffer
  init(buffer: CallBuffer) { self.buffer = buffer }
  public mutating func doneRecording() throws {}
}
extension PortableEncoder {
  public mutating func recordArgument<Value: PortableSerializationRequirement>(
      _ argument: RemoteCallArgument<Value>) throws {
    appendField(argument.value.toWireBytes(), to: &buffer.argBytes)
  }
}

public struct PortableDecoder: DistributedTargetInvocationDecoder {
  let buffer: CallBuffer
  var offset = 0
  // The decoder carries the actor system so that a distributed-actor argument
  // can be resolved from its serialized id (see `fromWireBytes`).
  let system: PortableRoundtripActorSystem
  init(buffer: CallBuffer, system: PortableRoundtripActorSystem) {
    self.buffer = buffer
    self.system = system
  }
}
extension PortableDecoder {
  public mutating func decodeNextArgument<Argument: PortableSerializationRequirement>() throws -> Argument {
    guard let field = takeField(buffer.argBytes, &offset) else { fatalError("wire underflow") }
    return try Argument.fromWireBytes(field, system: system)
  }
}

public struct PortableResultHandler: DistributedTargetInvocationResultHandler {
  let buffer: CallBuffer
  init(buffer: CallBuffer) { self.buffer = buffer }
  public func onReturnVoid() async throws {}
  public func onThrow(error: any Error) async throws { fatalError("threw in handler") }
}
extension PortableResultHandler {
  public func onReturn<Success: PortableSerializationRequirement>(_ value: Success) async throws {
    appendField(value.toWireBytes(), to: &buffer.returnBytes)
  }
}

public final class PortableRoundtripActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = PortableActorID
  #if $Embedded
  public typealias SerializationRequirement = PortableSerializationRequirement
  #else
  public typealias SerializationRequirement = Codable
  #endif
  public typealias InvocationEncoder = PortableEncoder
  public typealias InvocationDecoder = PortableDecoder
  public typealias ResultHandler = PortableResultHandler

  // Each hosted actor's monomorphized receive entrypoint, keyed by id. Embedded
  // has no existential-opening or metadata dispatch, so instead of storing an
  // `any DistributedActor` we store a thunk that closes over the concrete actor.
  public typealias LocalDispatch =
    (borrowing RemoteCallTarget, inout InvocationDecoder, ResultHandler) async throws -> Void
  var active: [ActorID: LocalDispatch] = [:]
  var nextID: UInt64 = 1

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ActorSystem == PortableRoundtripActorSystem {
    return nil // always resolve as remote: calls route through remoteCall
  }
  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ActorSystem == PortableRoundtripActorSystem {
    defer { nextID += 1 }
    return ActorID(id: nextID)
  }
  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ActorSystem == PortableRoundtripActorSystem {
    active[actor.id] = { [self] target, decoder, handler in
      // Same entry point the non-embedded system uses; in Embedded Swift the
      // `@_transparent` forwarder is inlined into this generic closure, so it
      // lowers to the actor's `_executeDistributedTarget` witness call
      try await self.executeDistributedTarget(
          on: actor, target: target, invocationDecoder: &decoder, handler: handler)
    }
  }
  public func resignID(_ id: ActorID) {}
  public func makeInvocationEncoder() -> InvocationEncoder { .init(buffer: CallBuffer()) }

  public func remoteCall<Act, Err, Res>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type, returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID,
            Err: Error, Res: SerializationRequirement {
    print("[swift] remoteCall reached")
    guard let dispatch = active[actor.id] else { fatalError("no local actor hosted") }

    // ==================== NETWORK: request bytes -> callee ====================
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = invocation.buffer.argBytes // value-type copy of the bytes
    var decoder = PortableDecoder(buffer: requestBuffer, system: self)
    let handler = PortableResultHandler(buffer: requestBuffer)
    try await dispatch(target, &decoder, handler)

    // ==================== NETWORK: response bytes -> caller ===================
    let responseBuffer = CallBuffer()
    responseBuffer.argBytes = requestBuffer.returnBytes
    var responseDecoder = PortableDecoder(buffer: responseBuffer, system: self)
    return try responseDecoder.decodeNextArgument()
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    fatalError("not exercised by portable roundtrip tests")
  }
}

#else

// ==== ----------------------------------------------------------------------
// MARK: Ordinary-Swift per-value serialization

// Ordinary Swift binds `SerializationRequirement` to `Codable` and does a
// genuine encode/decode with Foundation's JSON coders. The value is boxed so
// the top level is always a JSON object (avoids top-level-fragment concerns).
import Foundation

struct Box<T: Codable>: Codable { let value: T }

func toWireBytes<T: Codable>(_ value: T) -> [UInt8] {
  Array(try! JSONEncoder().encode(Box(value: value)))
}
func fromWireBytes<T: Codable>(_ bytes: [UInt8], as type: T.Type) -> T {
  try! JSONDecoder().decode(Box<T>.self, from: Data(bytes)).value
}

public struct PortableEncoder: DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Codable
  let buffer: CallBuffer
  init(buffer: CallBuffer) { self.buffer = buffer }
  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  public mutating func recordArgument<Value: Codable>(_ argument: RemoteCallArgument<Value>) throws {
    appendField(toWireBytes(argument.value), to: &buffer.argBytes)
  }
  public mutating func recordReturnType<R: Codable>(_ type: R.Type) throws {}
  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  public mutating func doneRecording() throws {}
}

public final class PortableDecoder: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Codable
  let buffer: CallBuffer
  var offset = 0
  init(buffer: CallBuffer) { self.buffer = buffer }
  public func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  public func decodeNextArgument<Argument: Codable>() throws -> Argument {
    guard let field = takeField(buffer.argBytes, &offset) else { fatalError("wire underflow") }
    return fromWireBytes(field, as: Argument.self)
  }
  public func decodeReturnType() throws -> Any.Type? { nil }
  public func decodeErrorType() throws -> Any.Type? { nil }
}

public struct PortableResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Codable
  let buffer: CallBuffer
  init(buffer: CallBuffer) { self.buffer = buffer }
  public func onReturn<Success: Codable>(value: Success) async throws {
    appendField(toWireBytes(value), to: &buffer.returnBytes)
  }
  public func onReturnVoid() async throws {}
  public func onThrow<Err: Error>(error: Err) async throws { fatalError("threw in handler") }
}

public final class PortableRoundtripActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = PortableActorID
  public typealias SerializationRequirement = Codable
  public typealias InvocationEncoder = PortableEncoder
  public typealias InvocationDecoder = PortableDecoder
  public typealias ResultHandler = PortableResultHandler

  var activeActors: [ActorID: any DistributedActor] = [:]
  var nextID: UInt64 = 1

  // How this system behaves when a call is made on a remote reference. Cross-
  // process tests need one side to serialize a request and stop, so a separately
  // built receiver - e.g. an Embedded server - can pick it up. `.inProcess` (the
  // default) keeps the ordinary in-process loopback used by the roundtrip tests
  public enum CrossProcessSimulatedMode {
    // Dispatch the call locally, in-process (default)
    case inProcess
    // Sender: write the outgoing request wire (target identifier + serialized
    // arguments) to `messageWritePath` and `exit(0)` - the response is produced
    // later by a separately built receiver reading that file
    case writeAndExit(messageWritePath: String)
  }
  public var crossProcessSimulatedMode: CrossProcessSimulatedMode = .inProcess

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ID == ActorID {
    return nil // always resolve as remote: calls route through remoteCall
  }
  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ID == ActorID {
    defer { nextID += 1 }
    return ActorID(id: nextID)
  }
  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {
    activeActors[actor.id] = actor
  }
  public func resignID(_ id: ActorID) {}
  public func makeInvocationEncoder() -> InvocationEncoder { .init(buffer: CallBuffer()) }

  public func remoteCall<Act, Err, Res>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing errorType: Err.Type, returning returnType: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID, Err: Error, Res: SerializationRequirement {
    // Sender mode: serialize the request out to a file and stop. The response
    // will be produced by a separately built receiver in a later step, so this
    // one-shot send never returns a `Res`
    if case .writeAndExit(let path) = crossProcessSimulatedMode {
      var wire: [UInt8] = []
      appendField(Array(target.identifier.utf8), to: &wire)  // field 0: mangled target id
      wire.append(contentsOf: invocation.buffer.argBytes)    // fields 1...n: the arguments
      // Write atomically so a reader in the next step never sees a partial file
      try Data(wire).write(to: URL(fileURLWithPath: path), options: .atomic)
      print("[swift] client sent request: " +
            String(decoding: invocation.buffer.argBytes, as: UTF8.self))
      exit(0)
    }
    print("[swift] remoteCall reached")
    guard let anyActor = activeActors[actor.id] else { fatalError("no local actor hosted") }
    // ==================== NETWORK: request bytes -> callee ====================
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = invocation.buffer.argBytes // value-type copy of the bytes
    let resultBuffer = CallBuffer()

    func doIt<A: DistributedActor>(active: A) async throws -> Res {
      var decoder = PortableDecoder(buffer: requestBuffer)
      let handler = PortableResultHandler(buffer: resultBuffer)
      try await executeDistributedTarget(
          on: active, target: target, invocationDecoder: &decoder, handler: handler)
      // ================== NETWORK: response bytes -> caller ==================
      let responseBuffer = CallBuffer()
      responseBuffer.argBytes = resultBuffer.returnBytes
      let responseDecoder = PortableDecoder(buffer: responseBuffer)
      return try responseDecoder.decodeNextArgument()
    }
    return try await _openExistential(anyActor, do: doIt)
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing errorType: Err.Type
  ) async throws where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    fatalError("not exercised by portable roundtrip tests")
  }
}

#endif

public typealias DefaultDistributedActorSystem = PortableRoundtripActorSystem
