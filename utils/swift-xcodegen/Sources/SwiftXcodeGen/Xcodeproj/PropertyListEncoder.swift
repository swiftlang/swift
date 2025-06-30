//===--- PropertyListEncoder.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension PropertyList {
  static func encode<T: Encodable>(_ x: T) throws -> PropertyList {
    let encoder = _Encoder()
    try x.encode(to: encoder)
    return encoder.result.value
  }
}

fileprivate extension PropertyList {
  final class Result {
    enum Underlying {
      case empty
      case string(String)
      case array([PropertyList])
      case dictionary([String: PropertyList])
    }
    fileprivate var underlying: Underlying = .empty

    var isEmpty: Bool {
      if case .empty = underlying { return true } else { return false }
    }

    @discardableResult
    func makeDictionary() -> Self {
      precondition(isEmpty)
      underlying = .dictionary([:])
      return self
    }

    @discardableResult
    func makeArray() -> Self {
      precondition(isEmpty)
      underlying = .array([])
      return self
    }

    @discardableResult
    func makeString(_ str: String) -> Self {
      precondition(isEmpty)
      underlying = .string(str)
      return self
    }

    var value: PropertyList {
      switch underlying {
      case .empty:
        fatalError("Didn't encode anything?")
      case .array(let array):
        return .array(array)
      case .dictionary(let dictionary):
        return .dictionary(dictionary)
      case .string(let str):
        return .string(str)
      }
    }

    private var _array: [PropertyList] {
      guard case .array(let arr) = underlying else {
        fatalError("Must be array")
      }
      return arr
    }

    var array: [PropertyList] {
      _read {
        yield _array
      }
      _modify {
        // Avoid a COW.
        var arr = _array
        underlying = .empty
        defer {
          underlying = .array(arr)
        }
        yield &arr
      }
    }

    private var _dictionary: [String: PropertyList] {
      guard case .dictionary(let dict) = underlying else {
        fatalError("Must be dictionary")
      }
      return dict
    }

    var dictionary: [String: PropertyList] {
      _read {
        yield _dictionary
      }
      _modify {
        // Avoid a COW.
        var dict = _dictionary
        underlying = .empty
        defer {
          underlying = .dictionary(dict)
        }
        yield &dict
      }
    }
  }

  struct _Encoder: Encoder {
    var userInfo: [CodingUserInfoKey: Any] { [:] }
    var codingPath: [CodingKey] { fatalError("Unsupported") }

    var result = Result()
    init() {}

    func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key> {
      .init(KeyedContainer<Key>(result: result.makeDictionary()))
    }

    func unkeyedContainer() -> UnkeyedEncodingContainer {
      UnkeyedContainer(result: result.makeArray())
    }
    func singleValueContainer() -> SingleValueEncodingContainer {
      SingleValueContainer(result: result)
    }
  }
}

extension PropertyList {
  fileprivate struct KeyedContainer<Key: CodingKey>: KeyedEncodingContainerProtocol {
    var codingPath: [CodingKey] { fatalError("Unsupported") }
    var result: Result

    mutating func encode(_ value: String, forKey key: Key) {
      result.dictionary[key.stringValue] = .string(value)
    }

    mutating func encode<T: Encodable>(_ value: T, forKey key: Key) throws {
      result.dictionary[key.stringValue] = try .encode(value)
    }

    mutating func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer { fatalError("Unsupported") }
    mutating func nestedContainer<NestedKey>(
      keyedBy keyType: NestedKey.Type,
      forKey key: Key
    ) -> KeyedEncodingContainer<NestedKey> { fatalError("Unsupported") }
    mutating func superEncoder(forKey key: Key) -> Encoder { fatalError("Unsupported") }
    mutating func superEncoder() -> Encoder { fatalError("Unsupported") }
    mutating func encodeNil(forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: Bool, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: Double, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: Float, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: Int, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: Int8, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: Int16, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: Int32, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: Int64, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt8, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt16, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt32, forKey key: Key) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt64, forKey key: Key) { fatalError("Unsupported") }
  }

  fileprivate struct UnkeyedContainer: UnkeyedEncodingContainer {
    var codingPath: [CodingKey] { fatalError("Unsupported") }

    var result: Result
    var count: Int {
      result.array.count
    }

    mutating func encode(_ value: String) {
      result.array.append(.string(value))
    }

    mutating func encode<T: Encodable>(_ value: T) throws {
      result.array.append(try .encode(value))
    }

    mutating func nestedContainer<NestedKey: CodingKey>(
      keyedBy keyType: NestedKey.Type
    ) -> KeyedEncodingContainer<NestedKey> { fatalError("Unsupported") }
    mutating func nestedUnkeyedContainer() -> UnkeyedEncodingContainer { fatalError("Unsupported") }
    mutating func superEncoder() -> Encoder { fatalError("Unsupported") }
    mutating func encodeNil() { fatalError("Unsupported") }
    mutating func encode(_ value: Bool) { fatalError("Unsupported") }
    mutating func encode(_ value: Double) { fatalError("Unsupported") }
    mutating func encode(_ value: Float) { fatalError("Unsupported") }
    mutating func encode(_ value: Int) { fatalError("Unsupported") }
    mutating func encode(_ value: Int8) { fatalError("Unsupported") }
    mutating func encode(_ value: Int16) { fatalError("Unsupported") }
    mutating func encode(_ value: Int32) { fatalError("Unsupported") }
    mutating func encode(_ value: Int64) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt8) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt16) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt32) { fatalError("Unsupported") }
    mutating func encode(_ value: UInt64) { fatalError("Unsupported") }
  }

  fileprivate struct SingleValueContainer: SingleValueEncodingContainer {
    var codingPath: [CodingKey] { fatalError("Unsupported") }
    var result: Result

    mutating func encode<T: Encodable>(_ value: T) throws {
      let encoder = _Encoder()
      try value.encode(to: encoder)
      result.underlying = encoder.result.underlying
    }

    mutating func encode(_ value: String) throws {
      result.makeString(value)
    }

    mutating func encodeNil() throws { fatalError("Unsupported") }
    mutating func encode(_ value: Bool) throws { fatalError("Unsupported") }
    mutating func encode(_ value: Double) throws { fatalError("Unsupported") }
    mutating func encode(_ value: Float) throws { fatalError("Unsupported") }
    mutating func encode(_ value: Int) throws { fatalError("Unsupported") }
    mutating func encode(_ value: Int8) throws { fatalError("Unsupported") }
    mutating func encode(_ value: Int16) throws { fatalError("Unsupported") }
    mutating func encode(_ value: Int32) throws { fatalError("Unsupported") }
    mutating func encode(_ value: Int64) throws { fatalError("Unsupported") }
    mutating func encode(_ value: UInt) throws { fatalError("Unsupported") }
    mutating func encode(_ value: UInt8) throws { fatalError("Unsupported") }
    mutating func encode(_ value: UInt16) throws { fatalError("Unsupported") }
    mutating func encode(_ value: UInt32) throws { fatalError("Unsupported") }
    mutating func encode(_ value: UInt64) throws { fatalError("Unsupported") }
  }
}
