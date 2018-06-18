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

/// A type that can be deserialized from ByteTree into a scalar value that
/// doesn't have any child nodes
protocol ByteTreeScalarDecodable {
  /// Construct the scalar value from the given serialized data
  ///
  /// - Parameters:
  ///   - pointer: The pointer pointing to the start of the serialized data
  ///   - size: The length of the serialized data in bytes
  /// - Returns: The deserialized value
  static func read(from pointer: UnsafeRawPointer, size: Int) -> Self
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
  static func read(from reader: ByteTreeObjectReader, numFields: Int) -> Self
}

// MARK: - Reader objects

// Implicitly create reference semantics for a stack allocated and
// non-reference counted object by creating a ByteTreeObjectReaderImpl on the
// stack and passing a pointer to it around, wrapped in another struct to
// increase ease of use.

// FIXME: We should be able to remove this workaround once we can specify memory
// ownership
struct ByteTreeObjectReader {
  private let impl: UnsafeMutablePointer<ByteTreeObjectReaderImpl>

  init(impl: UnsafeMutablePointer<ByteTreeObjectReaderImpl>) {
    self.impl = impl
  }

  /// Read the field at the given index as the specified type. All indicies must
  /// be read in order starting with 0. Skipping an index will result in a
  /// runtime assertion error. To discard a field use `discardField(:)`.
  ///
  /// - Parameters:
  ///   - objectType: The type as which this field should be read
  ///   - index: The index of this field
  /// - Returns: The decoded field
  func readField<FieldType: ByteTreeScalarDecodable>(
    _ objectType: FieldType.Type, index: Int
  ) -> FieldType {
    return impl.pointee.readField(objectType, index: index)
  }

  /// Read the field at the given index as the specified type. All indicies must
  /// be read in order starting with 0. Skipping an index will result in a
  /// runtime assertion error. To discard a field use `discardField(:)`.
  ///
  /// - Parameters:
  ///   - objectType: The type as which this field should be read
  ///   - index: The index of this field
  /// - Returns: The decoded field
  func readField<FieldType: ByteTreeObjectDecodable>(
    _ objectType: FieldType.Type, index: Int
  ) -> FieldType {
    return impl.pointee.readField(objectType, index: index)
  }

  /// Read and immediately discard the field at the specified index. This
  /// advances the reader by one field so that the next field can be read.
  ///
  /// - Parameter index: The index of the field that shall be discarded
  func discardField(index: Int) {
    impl.pointee.discardField(index: index)
  }
}

/// Helper object for reading objects out a ByteTree. Keeps track that fields
/// are not read out of order and discards all trailing fields that were present
/// in the binary format but were not handled when reading the object.
struct ByteTreeObjectReaderImpl {
  /// The reader that holds a reference to the data from which the object is
  /// read
  private let reader: ByteTreeReader

  /// The number of fields this object is expected to have
  private let numFields: Int

  /// The index of the field that is expected to be read next.
  private var nextIndex: Int = 0

  fileprivate init(reader: ByteTreeReader, numFields: Int) {
    self.reader = reader
    self.numFields = numFields
  }

  private mutating func advanceAndValidateIndex(_ index: Int) {
    assert(index == nextIndex, "Reading fields out of order")
    assert(index < numFields)
    nextIndex += 1
  }

  // Implementation of `ByteTreeObjectReader.readField`
  mutating func readField<FieldType: ByteTreeScalarDecodable>(
    _ objectType: FieldType.Type, index: Int
  ) -> FieldType {
    advanceAndValidateIndex(index)
    return reader.read(objectType)
  }

  // Implementation of `ByteTreeObjectReader.readField`
  mutating func readField<FieldType: ByteTreeObjectDecodable>(
    _ objectType: FieldType.Type, index: Int
  ) -> FieldType {
    advanceAndValidateIndex(index)
    return reader.read(objectType)
  }

  // Implementation of `ByteTreeObjectReader.discardField`
  mutating func discardField(index: Int) {
    advanceAndValidateIndex(index)
    reader.discardField()
  }

  // Since we do manual memory management for ByteTreeObjectReaderImpl we need
  // to call its destructor manually.
  // FIXME: Change this to deinit when ByteTreeObjectReader becomes a class
  mutating func finalize() {
    // Discard all fields that have not been read
    while nextIndex < numFields {
      discardField(index: nextIndex)
    }
  }
}

// Implicitly create reference semantics for a stack allocated and
// non-reference counted object by creating a ByteTreeObjectReaderImpl on the
// stack and passing a pointer to it around, wrapped in another struct to
// increase ease of use.

// FIXME: We should be able to remove this workaround once we can specify memory
// ownership
struct ByteTreeReader {
  /// The type as which the protocol version is encoded in ByteTree
  typealias ProtocolVersion = UInt32

  let impl: UnsafeMutablePointer<ByteTreeReaderImpl>

  init(impl: UnsafeMutablePointer<ByteTreeReaderImpl>) {
    self.impl = impl
  }

  // MARK: Public entrance function

  /// Deserialize an object tree from the ByteTree data at the given memory
  /// location.
  ///
  /// - Parameters:
  ///   - rootObjectType: The type of the root object in the deserialized tree
  ///   - pointer: The memory location at which the serialized data resides
  ///   - protocolVerisonValidation: A callback to determine if the data can be
  ///       read, based on the format's protocol version. If the callback
  ///       returns `false`, `nil` will be returned and reading aborded.
  /// - Returns: The deserialized tree or `nil` if protocol version validation
  ///            failed
  static func read<T: ByteTreeObjectDecodable>(
    _ rootObjectType: T.Type, from pointer: UnsafeRawPointer,
    protocolVersionValidation versionValidate: (ProtocolVersion) -> Bool
  ) -> T? {
    return ByteTreeReaderImpl.read(rootObjectType, from: pointer,
                                   protocolVersionValidation: versionValidate)
  }

  /// Read the next field in the tree as an object of the specified type.
  ///
  /// - Parameter objectType: The type as which the next field shall be read
  /// - Returns: The deserialized object
  fileprivate func read<T: ByteTreeObjectDecodable>(
    _ objectType: T.Type
  ) -> T {
    return impl.pointee.read(objectType)
  }

  /// Read the next field in the tree as a scalar of the specified type.
  ///
  /// - Parameter scalarType: The type as which the field shall be read
  /// - Returns: The deserialized scalar
  fileprivate func read<T: ByteTreeScalarDecodable>(
    _ scalarType: T.Type
  ) -> T {
    return impl.pointee.read(scalarType)
  }

  /// Discard the next scalar field, advancing the pointer to the next field
  fileprivate func discardField() {
    impl.pointee.discardField()
  }
}

/// Reader for reading the ByteTree format into Swift objects
struct ByteTreeReaderImpl {
  /// A pointer pointing to the next byte of serialized data to be read
  private var pointer: UnsafeRawPointer

  private init(pointer: UnsafeRawPointer) {
    self.pointer = pointer
  }

  // MARK: Public entrance function

  /// Implementation of `ByteTreeReader.read`
  static func read<T: ByteTreeObjectDecodable>(
    _ rootObjectType: T.Type, from pointer: UnsafeRawPointer,
    protocolVersionValidation: (ByteTreeReader.ProtocolVersion) -> Bool
  ) -> T? {
    var reader = ByteTreeReaderImpl(pointer: pointer)
    if !reader.readAndValidateProtocolVersion(protocolVersionValidation) {
      return nil
    }
    return reader.read(rootObjectType)
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

  /// Read the number of fields in an object or the binary length of a scalar
  /// field.
  ///
  /// - Returns: The read value
  private mutating func readFieldLength() -> Int {
    return Int(UInt32(littleEndian: readRaw(UInt32.self)))
  }

  /// Read the protocol version and validate that it can be read using the given
  /// callback.
  ///
  /// - Parameter validationCallback: A callback that determines if the given
  ///             protocol version can be read
  private mutating func readAndValidateProtocolVersion(
    _ validationCallback: (ByteTreeReader.ProtocolVersion) -> Bool
  ) -> Bool {
    let protocolVersion = ByteTreeReader.ProtocolVersion(littleEndian:
      readRaw(ByteTreeReader.ProtocolVersion.self))
    let result = validationCallback(protocolVersion)
    return result
  }

  /// Implementation of `ByteTreeReader.read`
  fileprivate mutating func read<T: ByteTreeObjectDecodable>(
    _ objectType: T.Type
  ) -> T {
    let numFields = readFieldLength()
    return withUnsafeMutablePointer(to: &self) { (selfPointer) in
      let reader = ByteTreeReader(impl: selfPointer)
      var objectReaderImpl = ByteTreeObjectReaderImpl(reader: reader,
                                                      numFields: numFields)
      defer {
        objectReaderImpl.finalize()
      }
      return withUnsafeMutablePointer(to: &objectReaderImpl) { (impl) in
        let objectReader = ByteTreeObjectReader(impl: impl)
        return T.read(from: objectReader, numFields: numFields)
      }
    }
  }

  /// Implementation of `ByteTreeReader.read`
  fileprivate mutating func read<T: ByteTreeScalarDecodable>(
    _ scalarType: T.Type
  ) -> T {
    let fieldSize = readFieldLength()
    defer {
      pointer = pointer.advanced(by: fieldSize)
    }
    return T.read(from: pointer, size: fieldSize)
  }

  /// Implementation of `ByteTreeReader.discardField`
  fileprivate mutating func discardField() {
    // FIXME: This can currently only discard scalar fields. Object fields
    // should also be discardable
    let fieldSize = readFieldLength()
    pointer = pointer.advanced(by: fieldSize)
  }
}

// MARK: - Common scalar type conformances

// Implemenation for reading an integer from memory to be shared between
// multiple types
extension ByteTreeScalarDecodable where Self : FixedWidthInteger {
  static func read(from pointer: UnsafeRawPointer, size: Int) -> Self {
    assert(size == MemoryLayout<Self>.size)
    return pointer.bindMemory(to: Self.self, capacity: 1).pointee
  }
}

extension UInt8: ByteTreeScalarDecodable {}
extension UInt16: ByteTreeScalarDecodable {}
extension UInt32: ByteTreeScalarDecodable {}

extension String: ByteTreeScalarDecodable {
  static func read(from pointer: UnsafeRawPointer, size: Int) -> String {
    let data = Data(bytes: pointer, count: size)
    return String(data: data, encoding: .utf8)!
  }
}

extension Optional: ByteTreeObjectDecodable
  where
  Wrapped: ByteTreeObjectDecodable {
  static func read(from reader: ByteTreeObjectReader, numFields: Int) ->
    Optional<Wrapped> {
    if numFields == 0 {
      return nil
    } else {
      return Wrapped.read(from: reader, numFields: numFields)
    }
  }
}

extension Array: ByteTreeObjectDecodable
  where
  Element: ByteTreeObjectDecodable {
  static func read(from reader: ByteTreeObjectReader, numFields: Int) ->
    Array<Element> {
    return (0..<numFields).map {
      return reader.readField(Element.self, index: $0)
    }
  }
}
