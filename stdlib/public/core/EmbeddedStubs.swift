//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// String

@_unavailableInEmbedded
public struct String: Hashable { 
    public var utf8CString: ContiguousArray<CChar> { fatalError() }
    public init() {}
    public init(validatingCString: UnsafePointer<CChar>) { fatalError() }
}

@_unavailableInEmbedded
extension String {
  public init<Subject>(describing instance: Subject) { fatalError() }
  public init<Subject>(reflecting instance: Subject) { fatalError() }
}

@_unavailableInEmbedded
extension String {
  public static func + (lhs: String, rhs: String) -> String { fatalError() }
  public static func += (lhs: inout String, rhs: String) { fatalError() }
}

@_unavailableInEmbedded
extension String {
  public var isContiguousUTF8: Bool { fatalError() }
  public mutating func makeContiguousUTF8() { fatalError() }
  public mutating func withUTF8<R>(_ body: (UnsafeBufferPointer<UInt8>) throws -> R) rethrows -> R { fatalError() }
  public var utf8: ContiguousArray<UInt8> { fatalError() }
  public var utf16: ContiguousArray<UInt16> { fatalError() }
  public var debugDescription: String { fatalError() }
}

@_unavailableInEmbedded
public func debugPrint(_ items: Any..., separator: String = " ", terminator: String = "\n") { fatalError() }

@_unavailableInEmbedded
public func debugPrint<Target: TextOutputStream>(_ items: Any..., separator: String = " ", terminator: String = "\n", to output: inout Target) { fatalError() }

@_unavailableInEmbedded
extension String: TextOutputStream {
  public mutating func write(_ other: String) { fatalError() }
  public mutating func _writeASCII(_ buffer: UnsafeBufferPointer<UInt8>) { fatalError() }
}

@_unavailableInEmbedded
extension String: _ExpressibleByBuiltinUnicodeScalarLiteral {
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) { fatalError() }
  public init(_ scalar: UInt32) { fatalError() }
}

@_unavailableInEmbedded
extension String: _ExpressibleByBuiltinExtendedGraphemeClusterLiteral {
  public init(_builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer, utf8CodeUnitCount: Builtin.Word, isASCII: Builtin.Int1) { fatalError() }
}

@_unavailableInEmbedded
extension String: _ExpressibleByBuiltinStringLiteral {
  public init(_builtinStringLiteral start: Builtin.RawPointer, utf8CodeUnitCount: Builtin.Word, isASCII: Builtin.Int1) { fatalError() }
}

@_unavailableInEmbedded
extension String: ExpressibleByStringLiteral {
  public init(stringLiteral value: String) { fatalError() }
}

@_unavailableInEmbedded
public protocol CustomStringConvertible {
 var description: String { get }
}

@_unavailableInEmbedded
public protocol CustomDebugStringConvertible {
  var debugDescription: String { get }
}

@_unavailableInEmbedded
public struct DefaultStringInterpolation: StringInterpolationProtocol, Sendable {
  public typealias StringLiteralType = String

  public init(literalCapacity: Int, interpolationCount: Int) { fatalError() }
  public mutating func appendLiteral(_ literal: StringLiteralType) { fatalError() }
  public mutating func appendInterpolation<T>(_: T) { fatalError() }

  internal __consuming func make() -> String { fatalError() }
}

@_unavailableInEmbedded
extension String {
  @inlinable
  @_effects(readonly)
  public init(stringInterpolation: DefaultStringInterpolation) { fatalError() }
}

@_unavailableInEmbedded
extension String: ExpressibleByStringInterpolation { }

@_unavailableInEmbedded
public protocol LosslessStringConvertible: CustomStringConvertible {
  init?(_ description: String)
}

@_unavailableInEmbedded
public protocol TextOutputStream {
  mutating func _lock()
  mutating func _unlock()
  mutating func write(_ string: String)
  mutating func _writeASCII(_ buffer: UnsafeBufferPointer<UInt8>)
}

@_unavailableInEmbedded
extension TextOutputStream {
  public mutating func _lock() {}
  public mutating func _unlock() {}
  public mutating func _writeASCII(_ buffer: UnsafeBufferPointer<UInt8>) {}
}

@_unavailableInEmbedded
public protocol TextOutputStreamable {
  func write<Target: TextOutputStream>(to target: inout Target)
}

@_unavailableInEmbedded
extension String {
  internal static func _fromUTF8Repairing(_ input: UnsafeBufferPointer<UInt8>) -> (result: String, repairsMade: Bool) { fatalError() }
}

@_unavailableInEmbedded
extension String {
  internal static func _fromASCII(_ input: UnsafeBufferPointer<UInt8>) -> String { fatalError() }
  internal static func _uncheckedFromUTF8( _ input: UnsafeBufferPointer<UInt8>) -> String { fatalError() }
  internal static func _uncheckedFromUTF8(_ input: UnsafeBufferPointer<UInt8>, isASCII: Bool) -> String { fatalError() }
  internal static func _uncheckedFromUTF8(_ input: UnsafeBufferPointer<UInt8>, asciiPreScanResult: Bool) -> String { fatalError() }
}

@_unavailableInEmbedded
extension String {
  public init<T: BinaryInteger>(_ value: T, radix: Int = 10, uppercase: Bool = false) { fatalError() }
}

/// Unicode

public enum Unicode {}

public typealias UTF8 = Unicode.UTF8
public typealias UTF16 = Unicode.UTF16

extension Unicode {
  public enum UTF8 {
  }
  public enum UTF16 {
  }
}

extension Unicode.UTF8 {
  public typealias CodeUnit = UInt8
}

extension Unicode.UTF16 {
  public typealias CodeUnit = UInt16
}

@_unavailableInEmbedded
extension String {
  public init(_ value: Unicode.Scalar) { fatalError() }
}

/// Codable

@_unavailableInEmbedded
public protocol Encodable {
  func encode(to encoder: any Encoder) throws
}

@_unavailableInEmbedded
public protocol Decodable {
  init(from decoder: any Decoder) throws
}

@_unavailableInEmbedded
public typealias Codable = Encodable & Decodable

@_unavailableInEmbedded
public protocol CodingKey { }

@_unavailableInEmbedded
public struct KeyedDecodingContainer<K: CodingKey> { }

@_unavailableInEmbedded
public struct KeyedEncodingContainer<K: CodingKey> { }

@_unavailableInEmbedded
public protocol UnkeyedDecodingContainer { 
  mutating func decode<T>(_ type: T.Type) throws -> T
}

@_unavailableInEmbedded
public protocol UnkeyedEncodingContainer { 
  mutating func encode<T>(_ value: T) throws
}

@_unavailableInEmbedded
public protocol SingleValueDecodingContainer { }

@_unavailableInEmbedded
public protocol SingleValueEncodingContainer { }

@_unavailableInEmbedded
public protocol Encoder {
  var codingPath: [any CodingKey] { get }
  func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key>
  func unkeyedContainer() -> any UnkeyedEncodingContainer
  func singleValueContainer() -> any SingleValueEncodingContainer
}

@_unavailableInEmbedded
public protocol Decoder {
  var codingPath: [any CodingKey] { get }
  func container<Key>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key>
  func unkeyedContainer() throws -> any UnkeyedDecodingContainer
  func singleValueContainer() throws -> any SingleValueDecodingContainer
}

@_unavailableInEmbedded
public enum DecodingError: Error {
  public struct Context: Sendable {
    public init(codingPath: [any CodingKey], debugDescription: String, underlyingError: Error? = nil) { fatalError() }
  }
  case typeMismatch(Any.Type, Context)
  case valueNotFound(Any.Type, Context)
  case keyNotFound(any CodingKey, Context)
  case dataCorrupted(Context)
}
