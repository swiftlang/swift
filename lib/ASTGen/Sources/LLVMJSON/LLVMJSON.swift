//===--- LLVMJSON.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import CBasicBridging

extension String {
  init(_ data: BridgedData) {
    let buffer = UnsafeBufferPointer(start: data.baseAddress, count: Int(data.size))
    self = buffer.withMemoryRebound(to: UInt8.self) { buffer in
      String(decoding: buffer, as: UTF8.self)
    }
  }
}

public struct LLVMJSON {
  /// Encode an `Encodable` value to JSON data, and call `body` is the buffer.
  /// Note that the buffer is valid onlu in `body`.
  public static func encoding<T: Encodable, R>(_ value: T, body: (UnsafeBufferPointer<Int8>) throws -> R) throws -> R {
    let valuePtr = JSON_newValue()
    defer { JSON_value_delete(valuePtr) }

    let encoder = LLVMJSONEncoding(to: valuePtr)
    try value.encode(to: encoder)

    var data: BridgedData = BridgedData()
    JSON_value_serialize(valuePtr, &data)
    assert(data.baseAddress != nil)
    defer { BridgedData_free(data) }
    let buffer = UnsafeBufferPointer(start: data.baseAddress, count: data.size)
    return try body(buffer)
  }

  /// Decode a JSON data to a Swift value.
  public static func decode<T: Decodable>(_ type: T.Type, from json: UnsafeBufferPointer<Int8>) throws -> T {
    let data = BridgedData(baseAddress: json.baseAddress, size: json.count)
    let valuePtr = JSON_deserializedValue(data)
    defer { JSON_value_delete(valuePtr) }

    let decoder = LLVMJSONDecoding(from: valuePtr)
    return try T.init(from: decoder)
  }
}

//===----------------------------------------------------------------------===//
// Decoding
//===----------------------------------------------------------------------===//

fileprivate struct LLVMJSONDecoding: Decoder {
  fileprivate struct KeyedContainer<Key: CodingKey> {
    var objectPtr: UnsafeMutableRawPointer
    var codingPath: [CodingKey]
  }
  fileprivate struct UnkeyedContainer {
    var arrayPtr: UnsafeMutableRawPointer
    var currentIndex: Int = 0
    var codingPath: [CodingKey]
  }
  fileprivate struct SingleValueContainer {
    var valuePtr: UnsafeMutableRawPointer
    var codingPath: [CodingKey]
  }

  var valuePtr: UnsafeMutableRawPointer
  var codingPath: [CodingKey]
  var userInfo: [CodingUserInfoKey : Any]

  init(from valuePtr: UnsafeMutableRawPointer, codingPath: [CodingKey] = [], userInfo: [CodingUserInfoKey : Any] = [:]) {
    self.valuePtr = valuePtr
    self.codingPath = codingPath
    self.userInfo = userInfo
  }

  func container<Key>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key> where Key : CodingKey {
    var objectPtr: UnsafeMutableRawPointer? = nil
    if JSON_value_getAsObject(valuePtr, &objectPtr) {
      throw DecodingError.typeMismatch(
        KeyedContainer<Key>.self,
        .init(codingPath: codingPath, debugDescription: "type mismatch"))
    }
    return KeyedDecodingContainer(KeyedContainer<Key>(objectPtr: objectPtr!, codingPath: codingPath))
  }

  func unkeyedContainer() throws -> UnkeyedDecodingContainer {
    var arrayPtr: UnsafeMutableRawPointer? = nil
    if JSON_value_getAsArray(valuePtr, &arrayPtr) {
      throw DecodingError.typeMismatch(
        UnkeyedContainer.self,
        .init(codingPath: codingPath, debugDescription: "type mismatch"))
    }
    return UnkeyedContainer(arrayPtr: arrayPtr!, codingPath: codingPath)
  }

  func singleValueContainer() throws -> SingleValueDecodingContainer {
    return SingleValueContainer(valuePtr: valuePtr, codingPath: codingPath)
  }
}

extension LLVMJSONDecoding.SingleValueContainer: SingleValueDecodingContainer {
  private func _typeMismatchError(_ type: Any.Type) -> DecodingError {
    DecodingError.typeMismatch(type, .init(codingPath: codingPath, debugDescription: "type misatch"))
  }

  func decodeNil() -> Bool {
    JSON_value_getAsNull(valuePtr)
  }

  func decode(_ type: Bool.Type) throws -> Bool {
    var result: Bool = false
    if JSON_value_getAsBoolean(valuePtr, &result) {
      throw _typeMismatchError(type)
    }
    return result
  }

  func decode(_ type: String.Type) throws -> String {
    var result: BridgedData = BridgedData()

    if JSON_value_getAsString(valuePtr, &result) {
      throw _typeMismatchError(type)
    }

    return String(result)
  }

  private func _decodeFloatingPoint<FP: BinaryFloatingPoint>(_ type: FP.Type) throws -> FP {
    var result: Double = 0
    if JSON_value_getAsDouble(valuePtr, &result) {
      throw _typeMismatchError(type)
    }
    return FP(result);
  }

  func decode(_ type: Double.Type) throws -> Double {
    try _decodeFloatingPoint(Double.self)
  }

  func decode(_ type: Float.Type) throws -> Float {
    try _decodeFloatingPoint(Float.self)
  }

  private func _decodeInteger<Integer: FixedWidthInteger>(_ type: Integer.Type) throws -> Integer {
    var result: Int64 = 0
    if JSON_value_getAsInteger(valuePtr, &result) {
      throw _typeMismatchError(type)
    }
    return Integer(result);
  }

  func decode(_ type: Int.Type) throws -> Int {
    try _decodeInteger(Int.self)
  }

  func decode(_ type: Int8.Type) throws -> Int8 {
    try _decodeInteger(Int8.self)
  }

  func decode(_ type: Int16.Type) throws -> Int16 {
    try _decodeInteger(Int16.self)
  }

  func decode(_ type: Int32.Type) throws -> Int32 {
    try _decodeInteger(Int32.self)
  }

  func decode(_ type: Int64.Type) throws -> Int64 {
    try _decodeInteger(Int64.self)
  }

  func decode(_ type: UInt.Type) throws -> UInt {
    try _decodeInteger(UInt.self)
  }

  func decode(_ type: UInt8.Type) throws -> UInt8 {
    try _decodeInteger(UInt8.self)
  }

  func decode(_ type: UInt16.Type) throws -> UInt16 {
    try _decodeInteger(UInt16.self)
  }

  func decode(_ type: UInt32.Type) throws -> UInt32 {
    try _decodeInteger(UInt32.self)
  }

  func decode(_ type: UInt64.Type) throws -> UInt64 {
    try _decodeInteger(UInt64.self)
  }

  func decode<T>(_ type: T.Type) throws -> T where T : Decodable {
    let decoder = LLVMJSONDecoding(from: valuePtr, codingPath: codingPath)
    return try T.init(from: decoder)
  }
}

extension LLVMJSONDecoding.KeyedContainer: KeyedDecodingContainerProtocol {
  typealias KeyedContainer = LLVMJSONDecoding.KeyedContainer
  typealias UnkeyedContainer = LLVMJSONDecoding.UnkeyedContainer
  typealias SingleContainer = LLVMJSONDecoding.SingleValueContainer

  var allKeys: [Key] {
    var keys: [Key] = []
    let size = JSON_object_getSize(objectPtr)
    keys.reserveCapacity(size)
    for i in 0 ..< size {
      let keyData = JSON_object_getKey(objectPtr, i)
      if let key = Key(stringValue: String(keyData)) {
        keys.append(key);
      }
    }
    return keys
  }

  func contains(_ key: Key) -> Bool {
    return JSON_object_hasKey(objectPtr, key.stringValue)
  }

  private func _getValue(forKey key: Key) -> UnsafeMutableRawPointer? {
    guard JSON_object_hasKey(objectPtr, key.stringValue) else {
      return nil
    }
    return JSON_object_getValue(objectPtr, key.stringValue)
  }

  private func _getValueOrThrow(forKey key: Key) throws -> UnsafeMutableRawPointer {
    guard let valuePtr = _getValue(forKey: key) else {
      throw DecodingError.keyNotFound(key, .init(codingPath: codingPath, debugDescription: "key not found"))
    }
    return valuePtr
  }

  private func _getSingle(forKey key: Key) throws -> SingleContainer {
    SingleContainer(valuePtr: try _getValueOrThrow(forKey: key),
                    codingPath: codingPath + [key])
  }


  private func _typeMismatchError(_ type: Any.Type, forKey key: Key) -> DecodingError {
    DecodingError.typeMismatch(type, .init(codingPath: codingPath + [key], debugDescription: "type misatch"))
  }

  func decodeNil(forKey key: Key) throws -> Bool {
    try _getSingle(forKey: key).decodeNil()
  }

  func decode(_ type: Bool.Type, forKey key: Key) throws -> Bool {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: String.Type, forKey key: Key) throws -> String {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: Double.Type, forKey key: Key) throws -> Double {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: Float.Type, forKey key: Key) throws -> Float {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: Int.Type, forKey key: Key) throws -> Int {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: Int8.Type, forKey key: Key) throws -> Int8 {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: Int16.Type, forKey key: Key) throws -> Int16 {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: Int32.Type, forKey key: Key) throws -> Int32 {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: Int64.Type, forKey key: Key) throws -> Int64 {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: UInt.Type, forKey key: Key) throws -> UInt {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: UInt8.Type, forKey key: Key) throws -> UInt8 {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: UInt16.Type, forKey key: Key) throws -> UInt16 {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: UInt32.Type, forKey key: Key) throws -> UInt32 {
    try _getSingle(forKey: key).decode(type)
  }

  func decode(_ type: UInt64.Type, forKey key: Key) throws -> UInt64 {
    try _getSingle(forKey: key).decode(type)
  }

  func decode<T>(_ type: T.Type, forKey key: Key) throws -> T where T : Decodable {
    try _getSingle(forKey: key).decode(type)
  }

  func decodeIfPresent(_ type: Bool.Type, forKey key: Key) throws -> Bool? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: String.Type, forKey key: Key) throws -> String? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: Double.Type, forKey key: Key) throws -> Double? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: Float.Type, forKey key: Key) throws -> Float? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: Int.Type, forKey key: Key) throws -> Int? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: Int8.Type, forKey key: Key) throws -> Int8? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: Int16.Type, forKey key: Key) throws -> Int16? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: Int32.Type, forKey key: Key) throws -> Int32? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: Int64.Type, forKey key: Key) throws -> Int64? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: UInt.Type, forKey key: Key) throws -> UInt? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: UInt8.Type, forKey key: Key) throws -> UInt8? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: UInt16.Type, forKey key: Key) throws -> UInt16? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: UInt32.Type, forKey key: Key) throws -> UInt32? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent(_ type: UInt64.Type, forKey key: Key) throws -> UInt64? {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func decodeIfPresent<T>(_ type: T.Type, forKey key: Key) throws -> T? where T : Decodable {
    guard contains(key) else { return nil }
    return try decode(type, forKey: key)
  }

  func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> where NestedKey : CodingKey {
    var objectPtr: UnsafeMutableRawPointer? = nil
    if JSON_value_getAsObject(try _getValueOrThrow(forKey: key), &objectPtr) {
      throw _typeMismatchError(KeyedDecodingContainer<NestedKey>.self, forKey: key)
    }

    return KeyedDecodingContainer(KeyedContainer<NestedKey>(objectPtr: objectPtr!, codingPath: codingPath + [key]))
  }

  func nestedUnkeyedContainer(forKey key: Key) throws -> UnkeyedDecodingContainer {
    var arrayPtr: UnsafeMutableRawPointer? = nil
    if JSON_value_getAsArray(try _getValueOrThrow(forKey: key), &arrayPtr) {
      throw _typeMismatchError(UnkeyedContainer.self, forKey: key)
    }

    return UnkeyedContainer(arrayPtr: arrayPtr!, codingPath: codingPath + [key])
  }

  func superDecoder() throws -> Decoder {
    fatalError("unimplemented")
  }

  func superDecoder(forKey key: Key) throws -> Decoder {
    fatalError("unimplemented")
  }
}

extension LLVMJSONDecoding.UnkeyedContainer: UnkeyedDecodingContainer {
  typealias KeyedContainer = LLVMJSONDecoding.KeyedContainer
  typealias UnkeyedContainer = LLVMJSONDecoding.UnkeyedContainer
  typealias SingleContainer = LLVMJSONDecoding.SingleValueContainer

  struct IndexKey: CodingKey {
    var intValue: Int?
    var stringValue: String { intValue!.description }
    init(intValue value: Int) { self.intValue = value }
    init?(stringValue: String) {
      return nil
    }
  }

  var count: Int? {
    Int(JSON_array_getSize(arrayPtr))
  }

  var isAtEnd: Bool {
    currentIndex == count
  }

  private mutating func _getValueOrThrow() throws -> UnsafeMutableRawPointer {
    guard !isAtEnd else {
      throw DecodingError.valueNotFound(
        Bool.self,
        .init(codingPath: codingPath, debugDescription: "tried to decode too many"))
    }
    let index = currentIndex
    currentIndex += 1
    return JSON_array_getValue(arrayPtr, numericCast(index))
  }

  private mutating func _getSingle() throws -> SingleContainer {
    SingleContainer(valuePtr: try _getValueOrThrow(),
                    codingPath: codingPath + [IndexKey(intValue: currentIndex)])
  }

  mutating func decodeNil() throws -> Bool {
    try _getSingle().decodeNil()
  }

  mutating func decode(_ type: Bool.Type) throws -> Bool {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: String.Type) throws -> String {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: Double.Type) throws -> Double {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: Float.Type) throws -> Float {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: Int.Type) throws -> Int {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: Int8.Type) throws -> Int8 {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: Int16.Type) throws -> Int16 {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: Int32.Type) throws -> Int32 {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: Int64.Type) throws -> Int64 {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: UInt.Type) throws -> UInt {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: UInt8.Type) throws -> UInt8 {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: UInt16.Type) throws -> UInt16 {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: UInt32.Type) throws -> UInt32 {
    try _getSingle().decode(type)
  }

  mutating func decode(_ type: UInt64.Type) throws -> UInt64 {
    try _getSingle().decode(type)
  }

  mutating func decode<T>(_ type: T.Type) throws -> T where T : Decodable {
    try _getSingle().decode(type)
  }

  private func _typeMismatchError(_ type: Any.Type) -> DecodingError {
    DecodingError.typeMismatch(
      type, .init(codingPath: codingPath + [IndexKey(intValue: currentIndex)],
                  debugDescription: "type misatch"))
  }

  mutating func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type) throws -> KeyedDecodingContainer<NestedKey> where NestedKey : CodingKey {
    var objectPtr: UnsafeMutableRawPointer? = nil
    let newPath = codingPath + [IndexKey(intValue: currentIndex)]
    if JSON_value_getAsObject(try _getValueOrThrow(), &objectPtr) {
      throw _typeMismatchError(KeyedDecodingContainer<NestedKey>.self)
    }

    return KeyedDecodingContainer(KeyedContainer(objectPtr: objectPtr!, codingPath: newPath))
  }

  mutating func nestedUnkeyedContainer() throws -> UnkeyedDecodingContainer {
    var arrayPtr: UnsafeMutableRawPointer? = nil
    let newPath = codingPath + [IndexKey(intValue: currentIndex)]
    if JSON_value_getAsArray(try _getValueOrThrow(), &arrayPtr) {
      throw _typeMismatchError(UnkeyedContainer.self)
    }

    return UnkeyedContainer(arrayPtr: arrayPtr!, codingPath: newPath)
  }

  mutating func superDecoder() throws -> Decoder {
    fatalError("unimplemented")
  }
}

//===----------------------------------------------------------------------===//
// Encoding
//===----------------------------------------------------------------------===//

fileprivate struct LLVMJSONEncoding: Encoder {
  fileprivate struct SingleValueContainer {
    var valuePtr: UnsafeMutableRawPointer
    var codingPath: [CodingKey]
  }
  fileprivate struct UnkeyedContainer {
    var arrayPtr: UnsafeMutableRawPointer
    var codingPath: [CodingKey]
  }
  fileprivate struct KeyedContainer<Key: CodingKey> {
    var objectPtr: UnsafeMutableRawPointer
    var codingPath: [CodingKey]
  }

  var valuePtr: UnsafeMutableRawPointer
  var codingPath: [CodingKey]
  var userInfo: [CodingUserInfoKey : Any]

  init(to valuePtr: UnsafeMutableRawPointer, codingPath: [CodingKey] = [], userInfo: [CodingUserInfoKey : Any] = [:]) {
    self.valuePtr = valuePtr
    self.codingPath = codingPath
    self.userInfo = userInfo
  }

  func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key> where Key : CodingKey {
    let objectPtr = JSON_value_emplaceNewObject(valuePtr)
    return KeyedEncodingContainer(KeyedContainer(objectPtr: objectPtr, codingPath: codingPath))
  }

  func unkeyedContainer() -> UnkeyedEncodingContainer {
    let arrayPtr = JSON_value_emplaceNewArray(valuePtr)
    return UnkeyedContainer(arrayPtr: arrayPtr, codingPath: codingPath)
  }

  func singleValueContainer() -> SingleValueEncodingContainer {
    return SingleValueContainer(valuePtr: valuePtr, codingPath: codingPath)
  }
}

extension LLVMJSONEncoding.KeyedContainer: KeyedEncodingContainerProtocol {
  typealias UnkeyedContainer = LLVMJSONEncoding.UnkeyedContainer
  typealias KeyedContainer = LLVMJSONEncoding.KeyedContainer

  mutating func encodeNil(forKey key: Key) throws {
    JSON_object_setNull(objectPtr, key.stringValue)
  }

  mutating func encode(_ value: Bool, forKey key: Key) throws {
    JSON_object_setBoolean(objectPtr, key.stringValue, value)
  }

  mutating func encode(_ value: String, forKey key: Key) throws {
    JSON_object_setString(objectPtr, key.stringValue, value)
  }

  mutating func encode(_ value: Double, forKey key: Key) throws {
    JSON_object_setDouble(objectPtr, key.stringValue, value)
  }

  mutating func encode(_ value: Float, forKey key: Key) throws {
    JSON_object_setDouble(objectPtr, key.stringValue, Double(value))
  }

  mutating func encode(_ value: Int, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode(_ value: Int8, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode(_ value: Int16, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode(_ value: Int32, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode(_ value: Int64, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode(_ value: UInt, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode(_ value: UInt8, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode(_ value: UInt16, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode(_ value: UInt32, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode(_ value: UInt64, forKey key: Key) throws {
    JSON_object_setInteger(objectPtr, key.stringValue, Int64(value))
  }

  mutating func encode<T>(_ value: T, forKey key: Key) throws where T : Encodable {
    let valuePtr = JSON_object_setNewValue(objectPtr, key.stringValue)
    let encoder = LLVMJSONEncoding(to: valuePtr, codingPath: codingPath + [key])
    try value.encode(to: encoder)
  }

  mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type, forKey key: Key) -> KeyedEncodingContainer<NestedKey> where NestedKey : CodingKey {
    let nestedObjectPtr = JSON_object_setNewObject(objectPtr, key.stringValue)
    return KeyedEncodingContainer(KeyedContainer(objectPtr: nestedObjectPtr, codingPath: codingPath + [key]))
  }

  mutating func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer {
    let nestedArrayPtr = JSON_object_setNewArray(objectPtr, key.stringValue)
    return UnkeyedContainer(arrayPtr: nestedArrayPtr, codingPath: codingPath + [key])
  }

  mutating func superEncoder() -> Encoder {
    fatalError("unimplemented")
  }

  mutating func superEncoder(forKey key: Key) -> Encoder {
    fatalError("unimplemented")
  }
}

extension LLVMJSONEncoding.UnkeyedContainer: UnkeyedEncodingContainer {
  typealias UnkeyedContainer = LLVMJSONEncoding.UnkeyedContainer
  typealias KeyedContainer = LLVMJSONEncoding.KeyedContainer

  struct IndexKey: CodingKey {
    var intValue: Int?
    var stringValue: String { intValue!.description }
    init(intValue value: Int) { self.intValue = value }
    init?(stringValue: String) {
      return nil
    }
  }

  var count: Int {
    return Int(JSON_array_getSize(arrayPtr))
  }

  mutating func encodeNil() throws {
    JSON_array_pushNull(arrayPtr)
  }

  mutating func encode(_ value: Bool) throws {
    JSON_array_pushBoolean(arrayPtr, value)
  }

  mutating func encode(_ value: String) throws {
    JSON_array_pushString(arrayPtr, value)
  }

  mutating func encode(_ value: Double) throws {
    JSON_array_pushDouble(arrayPtr, Double(value))
  }

  mutating func encode(_ value: Float) throws {
    JSON_array_pushDouble(arrayPtr, Double(value))
  }

  mutating func encode(_ value: Int) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode(_ value: Int8) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode(_ value: Int16) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode(_ value: Int32) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode(_ value: Int64) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode(_ value: UInt) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode(_ value: UInt8) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode(_ value: UInt16) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode(_ value: UInt32) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode(_ value: UInt64) throws {
    JSON_array_pushInteger(arrayPtr, Int64(value))
  }

  mutating func encode<T>(_ value: T) throws where T : Encodable {
    let key = IndexKey(intValue: self.count)
    let valuePtr = JSON_array_pushNewValue(arrayPtr)
    let encoder = LLVMJSONEncoding(to: valuePtr, codingPath: codingPath + [key])
    try value.encode(to: encoder)
  }

  mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type) -> KeyedEncodingContainer<NestedKey> where NestedKey : CodingKey {
    let key = IndexKey(intValue: self.count)
    let nestedObjectPtr = JSON_array_pushNewObject(arrayPtr)
    return KeyedEncodingContainer(KeyedContainer(objectPtr: nestedObjectPtr,
                                                 codingPath: codingPath + [key]))
  }

  mutating func nestedUnkeyedContainer() -> UnkeyedEncodingContainer {
    let key = IndexKey(intValue: self.count)
    let nestedArrayPtr = JSON_array_pushNewArray(arrayPtr)
    return UnkeyedContainer(arrayPtr: nestedArrayPtr, codingPath: codingPath + [key])
  }

  mutating func superEncoder() -> Encoder {
    fatalError("unsupported")
  }
}

extension LLVMJSONEncoding.SingleValueContainer: SingleValueEncodingContainer {
  mutating func encodeNil() throws {
    JSON_value_emplaceNull(valuePtr);
  }

  mutating func encode(_ value: Bool) throws {
    JSON_value_emplaceBoolean(valuePtr, value);
  }

  mutating func encode(_ value: String) throws {
    JSON_value_emplaceString(valuePtr, value);
  }

  mutating func encode(_ value: Double) throws {
    JSON_value_emplaceDouble(valuePtr, Double(value));
  }

  mutating func encode(_ value: Float) throws {
    JSON_value_emplaceDouble(valuePtr, Double(value));
  }

  mutating func encode(_ value: Int) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode(_ value: Int8) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode(_ value: Int16) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode(_ value: Int32) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode(_ value: Int64) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode(_ value: UInt) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode(_ value: UInt8) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode(_ value: UInt16) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode(_ value: UInt32) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode(_ value: UInt64) throws {
    JSON_value_emplaceInteger(valuePtr, Int64(value));
  }

  mutating func encode<T>(_ value: T) throws where T : Encodable {
    let encoder = LLVMJSONEncoding(to: valuePtr, codingPath: codingPath)
    try value.encode(to: encoder)
  }
}
