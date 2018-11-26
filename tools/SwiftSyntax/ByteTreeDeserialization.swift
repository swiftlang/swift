//===----- ByteTreeDeserialization.swift - Reading the ByteTree format ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation

// MARK: - ByteTree decoder protocols

struct ByteTreeUserInfoKey: Hashable {
  let rawValue: String

  init(rawValue: String) {
    self.rawValue = rawValue
  }
}

/// A type that can be deserialized from ByteTree into a scalar value that
/// doesn't have any child nodes
protocol ByteTreeScalarDecodable {
  /// Construct the scalar value from the given serialized data
  ///
  /// - Parameters:
  ///   - pointer: The pointer pointing to the start of the serialized data
  ///   - size: The length of the serialized data in bytes
  /// - Returns: The deserialized value
  static func read(from pointer: UnsafeRawPointer, size: Int,
                   userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>) -> Self
}

/// A type that can be deserialized from ByteTree into an object with child
/// nodes
protocol ByteTreeObjectDecodable {
  /// Construct a object from the serialized ByteTree object.
  ///
  /// The fields can be read from `reader` using the corresponding `readField`
  /// methods.
  ///
  /// - Parameters:
  ///   - reader: The reader from which the object's field's values can be read
  ///   - numFields: The number of fields that are present in the serialized
  ///                object
  /// - Returns: The deserialized object
  static func read(from reader: UnsafeMutablePointer<ByteTreeObjectReader>,
                   numFields: Int,
                   userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>) -> Self
}

// MARK: - Reader objects

/// Helper object for reading objects out a ByteTree. Keeps track that fields
/// are not read out of order and discards all trailing fields that were present
/// in the binary format but were not handled when reading the object.
struct ByteTreeObjectReader {
  /// The reader that holds a reference to the data from which the object is
  /// read
  private let reader: UnsafeMutablePointer<ByteTreeReader>

  /// The number of fields this object is expected to have
  private let numFields: Int

  /// The index of the field that is expected to be read next.
  private var nextIndex: Int = 0

  fileprivate init(reader: UnsafeMutablePointer<ByteTreeReader>,
                   numFields: Int) {
    self.reader = reader
    self.numFields = numFields
  }

  private mutating func advanceAndValidateIndex(_ index: Int) {
    assert(index == nextIndex, "Reading fields out of order")
    assert(index < numFields)
    nextIndex += 1
  }


  /// Read the field at the given index as the specified type. All indicies must
  /// be read in order starting with 0. Skipping an index will result in a
  /// runtime assertion error. To discard a field use `discardField(:)`.
  ///
  /// - Parameters:
  ///   - objectType: The type as which this field should be read
  ///   - index: The index of this field
  /// - Returns: The decoded field
  mutating func readField<FieldType: ByteTreeScalarDecodable>(
    _ objectType: FieldType.Type, index: Int
  ) -> FieldType {
    advanceAndValidateIndex(index)
    return reader.pointee.read(objectType)
  }

  /// Read the field at the given index as the specified type. All indicies must
  /// be read in order starting with 0. Skipping an index will result in a
  /// runtime assertion error. To discard a field use `discardField(:)`.
  ///
  /// - Parameters:
  ///   - objectType: The type as which this field should be read
  ///   - index: The index of this field
  /// - Returns: The decoded field
  mutating func readField<FieldType: ByteTreeObjectDecodable>(
    _ objectType: FieldType.Type, index: Int
  ) -> FieldType {
    advanceAndValidateIndex(index)
    return reader.pointee.read(objectType)
  }

  /// Read and immediately discard the field at the specified index. This
  /// advances the reader by one field so that the next field can be read.
  ///
  /// - Parameter index: The index of the field that shall be discarded
  mutating func discardField(index: Int) {
    advanceAndValidateIndex(index)
    reader.pointee.discardField()
  }

  fileprivate mutating func finalize() {
    // Discard all fields that have not been read
    while nextIndex < numFields {
      discardField(index: nextIndex)
    }
  }
}

/// Reader for reading the ByteTree format into Swift objects
struct ByteTreeReader {
  enum DeserializationError: Error, CustomStringConvertible {
    case versionValidationFailed(ByteTreeReader.ProtocolVersion)

    public var description: String {
      switch self {
      case .versionValidationFailed(let version):
        return "The serialized ByteTree version \(version) cannot be parsed " +
               "by this version of swiftSyntax"
      }
    }
  }

  /// The type as which the protocol version is encoded in ByteTree
  typealias ProtocolVersion = UInt32

  /// A pointer pointing to the next byte of serialized data to be read
  private var pointer: UnsafeRawPointer

  private var userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>

  private init(pointer: UnsafeRawPointer,
               userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>) {
    self.pointer = pointer
    self.userInfo = userInfo
  }

  // MARK: Public entrance function

  /// Deserialize an object tree from the ByteTree data at the given memory
  /// location.
  ///
  /// - Parameters:
  ///   - rootObjectType: The type of the root object in the deserialized tree
  ///   - pointer: The memory location at which the serialized data resides
  ///   - protocolVersionValidation: A callback to determine if the data can be
  ///       read, based on the format's protocol version. If the callback
  ///       returns `false` an error will be thrown
  /// - Returns: The deserialized tree or `nil` if protocol version validation
  ///            failed
  static func read<T: ByteTreeObjectDecodable>(
    _ rootObjectType: T.Type, from pointer: UnsafeRawPointer,
    userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>,
    protocolVersionValidation: (ProtocolVersion) -> Bool
  ) throws -> T {
    var reader = ByteTreeReader(pointer: pointer, userInfo: userInfo)
    try reader.readAndValidateProtocolVersion(protocolVersionValidation)
    return reader.read(rootObjectType)
  }

  /// Deserialize an object tree from the ByteTree data at the given memory
  /// location.
  ///
  /// - Parameters:
  ///   - rootObjectType: The type of the root object in the deserialized tree
  ///   - data: The data to deserialize
  ///   - protocolVersionValidation: A callback to determine if the data can be
  ///       read, based on the format's protocol version. If the callback
  ///       returns `false` an error will be thrown
  /// - Returns: The deserialized tree
  static func read<T: ByteTreeObjectDecodable>(
    _ rootObjectType: T.Type, from data: Data,
    userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>,
    protocolVersionValidation versionValidate: (ProtocolVersion) -> Bool
  ) throws -> T {
    return try data.withUnsafeBytes { (pointer: UnsafePointer<UInt8>) in
      let rawPointer = UnsafeRawPointer(pointer)
      return try ByteTreeReader.read(rootObjectType, from: rawPointer,
                                     userInfo: userInfo,
                                     protocolVersionValidation: versionValidate)
    }
  }


  // MARK: Internal read functions

  /// Cast the current pointer location to the given type and advance `pointer`
  /// to point behind the just read bytes.
  ///
  /// - Parameter type: The type as which the current data should be read
  /// - Returns: The read value
  private mutating func readRaw<T>(_ type: T.Type) -> T {
    let result = pointer.bindMemory(to: T.self, capacity: 1).pointee
    pointer = pointer.advanced(by: MemoryLayout<T>.size)
    return result
  }

  private mutating func readFieldLength() -> (isObject: Bool, length: Int) {
    let raw = UInt32(littleEndian: readRaw(UInt32.self))
    let isObject = (raw & (UInt32(1) << 31)) != 0
    let length = Int(raw & ~(UInt32(1) << 31))
    return (isObject, length)
  }


  /// Read the number of fields in an object.
  ///
  /// - Returns: The number of fields in the following object
  private mutating func readObjectLength() -> Int {
    let (isObject, length) = readFieldLength()
    assert(isObject)
    return length
  }

  /// Read the size of a scalar in bytes
  ///
  /// - Returns: The size of the following scalar in bytes
  private mutating func readScalarLength() -> Int {
    let (isObject, length) = readFieldLength()
    assert(!isObject)
    return length
  }

  /// Read the protocol version and validate that it can be read using the given
  /// callback.
  ///
  /// - Parameter validationCallback: A callback that determines if the given
  ///             protocol version can be read
  private mutating func readAndValidateProtocolVersion(
    _ validationCallback: (ProtocolVersion) -> Bool
  ) throws {
    let protocolVersion = ProtocolVersion(littleEndian: 
      readRaw(ProtocolVersion.self))
    let result = validationCallback(protocolVersion)
    if !result {
      throw DeserializationError.versionValidationFailed(protocolVersion)
    }
  }

  /// Read the next field in the tree as an object of the specified type.
  ///
  /// - Parameter objectType: The type as which the next field shall be read
  /// - Returns: The deserialized object
  fileprivate mutating func read<T: ByteTreeObjectDecodable>(
    _ objectType: T.Type
  ) -> T {
    let numFields = readObjectLength()
    var objectReader = ByteTreeObjectReader(reader: &self,
                                            numFields: numFields)
    defer {
      objectReader.finalize()
    }
    return T.read(from: &objectReader, numFields: numFields,
                  userInfo: userInfo)
  }

  /// Read the next field in the tree as a scalar of the specified type.
  ///
  /// - Parameter scalarType: The type as which the field shall be read
  /// - Returns: The deserialized scalar
  fileprivate mutating func read<T: ByteTreeScalarDecodable>(
    _ scalarType: T.Type
  ) -> T {
    let fieldSize = readScalarLength()
    defer {
      pointer = pointer.advanced(by: fieldSize)
    }
    return T.read(from: pointer, size: fieldSize, userInfo: userInfo)
  }

  /// Discard the next scalar field, advancing the pointer to the next field
  fileprivate mutating func discardField() {
    let (isObject, length) = readFieldLength()
    if isObject {
      // Discard object by discarding all its objects
      for _ in 0..<length {
        discardField()
      }
    } else {
      // Discard scalar
      pointer = pointer.advanced(by: length)
    }
  }
}

// MARK: - Common scalar type conformances

// Implemenation for reading an integer from memory to be shared between
// multiple types
extension ByteTreeScalarDecodable where Self : FixedWidthInteger {
  static func read(from pointer: UnsafeRawPointer, size: Int,
                   userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>
  ) -> Self {
    assert(size == MemoryLayout<Self>.size)
    return pointer.bindMemory(to: Self.self, capacity: 1).pointee
  }
}

extension UInt8: ByteTreeScalarDecodable {}
extension UInt16: ByteTreeScalarDecodable {}
extension UInt32: ByteTreeScalarDecodable {}

extension String: ByteTreeScalarDecodable {
  static func read(from pointer: UnsafeRawPointer, size: Int,
                   userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>
  ) -> String {
    let data = Data(bytes: pointer, count: size)
    return String(data: data, encoding: .utf8)!
  }
}

extension Optional: ByteTreeObjectDecodable
  where
  Wrapped: ByteTreeObjectDecodable {
  static func read(from reader: UnsafeMutablePointer<ByteTreeObjectReader>,
                   numFields: Int,
                   userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>
  ) -> Optional<Wrapped> {
    if numFields == 0 {
      return nil
    } else {
      return Wrapped.read(from: reader, numFields: numFields,
                          userInfo: userInfo)
    }
  }
}

extension Array: ByteTreeObjectDecodable
  where
  Element: ByteTreeObjectDecodable {
  static func read(from reader: UnsafeMutablePointer<ByteTreeObjectReader>,
                   numFields: Int,
                   userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>
  ) -> Array<Element> {
    return (0..<numFields).map {
      return reader.pointee.readField(Element.self, index: $0)
    }
  }
}
