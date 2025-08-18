//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A Unicode string value that is a collection of characters.
///
/// A string is a series of characters, such as "Swift", that forms a collection.
/// Strings in Swift are Unicode correct and locale insensitive, and are designed
/// to be efficient. The `String` type bridges with the Objective-C class `NSString`
/// and offers interoperability with C functions that works with strings.
@frozen
public struct String {
  @usableFromInline
  internal var _guts: _StringGuts

  /// Creates an empty string.
  @inlinable @inline(__always)
  public init() {
    _guts = _StringGuts()
  }

  @inlinable @inline(__always)
  internal init(_guts: _StringGuts) {
    self._guts = _guts
  }
}

// MARK: - String literals

extension String: ExpressibleByStringLiteral {
  /// The type of the string literal, `String`.
  public typealias StringLiteralType = String

  /// Creates an instance initialized to the given string value.
  @_transparent
  public init(stringLiteral value: String) {
    self = value
  }
}

extension String: ExpressibleByExtendedGraphemeClusterLiteral {
  /// The type of the extended grapheme cluster literal, `String`.
  public typealias ExtendedGraphemeClusterLiteralType = String

  /// Creates an instance initialized to the given extended grapheme cluster
  /// literal.
  @_transparent
  public init(extendedGraphemeClusterLiteral value: String) {
    self = value
  }
}

extension String: ExpressibleByUnicodeScalarLiteral {
  /// The type of the Unicode scalar literal, `String`.
  public typealias UnicodeScalarLiteralType = String

  /// Creates an instance initialized to the given Unicode scalar literal.
  @_transparent
  public init(unicodeScalarLiteral value: String) {
    self = value
  }
}

// MARK: - Basic properties

extension String {
  /// A Boolean value indicating whether a string has no characters.
  @inlinable
  public var isEmpty: Bool {
    return _guts.count == 0
  }

  /// The number of characters in a string.
  @inlinable
  public var count: Int {
    return _guts.count
  }
}

// MARK: - String concatenation

extension String {
  /// Creates a new string by concatenating the given strings.
  @_transparent
  public static func + (lhs: String, rhs: String) -> String {
    if lhs.isEmpty { return rhs }
    if rhs.isEmpty { return lhs }
    
    var result = lhs
    result.append(rhs)
    return result
  }

  /// Appends the given string to this string.
  @_transparent
  public static func += (lhs: inout String, rhs: String) {
    lhs.append(rhs)
  }

  /// Appends the given string to this string.
  @inlinable
  public mutating func append(_ other: String) {
    _guts.append(other._guts)
  }

  /// Appends the given character to the string.
  @inlinable
  public mutating func append(_ c: Character) {
    _guts.append(c)
  }
}

// MARK: - Equatable and Comparable

extension String: Equatable {
  /// Returns a Boolean value indicating whether two strings are equal.
  @inlinable
  public static func == (lhs: String, rhs: String) -> Bool {
    return lhs._guts.isEqual(to: rhs._guts)
  }
}

extension String: Comparable {
  /// Returns a Boolean value indicating whether the first string is ordered
  /// before the second in a lexicographical (dictionary) ordering.
  @inlinable
  public static func < (lhs: String, rhs: String) -> Bool {
    return lhs._guts.compare(with: rhs._guts) < 0
  }
}

// MARK: - Hashable conformance

extension String: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    _guts.hash(into: &hasher)
  }
}

// MARK: - CustomStringConvertible

extension String: CustomStringConvertible {
  /// The string itself.
  public var description: String { return self }
}

extension String: CustomDebugStringConvertible {
  /// A representation of the string that is suitable for debugging.
  public var debugDescription: String {
    var result = "\""
    for char in self {
      result.append(char.debugDescription)
    }
    result.append("\"")
    return result
  }
}

// MARK: - String interpolation

extension String: ExpressibleByStringInterpolation {
  /// The type used for string interpolation literals.
  public struct StringInterpolation: StringInterpolationProtocol {
    @usableFromInline
    internal var _result: String

    /// Creates an empty instance ready to be filled with string literal content.
    @inlinable
    public init(literalCapacity: Int, interpolationCount: Int) {
      _result = String()
    }

    /// Appends a literal string segment to the interpolation.
    @inlinable
    public mutating func appendLiteral(_ literal: String) {
      _result.append(literal)
    }

    /// Appends the result of the given expression to the interpolation.
    @inlinable
    public mutating func appendInterpolation<T>(_ value: T) where T : CustomStringConvertible {
      _result.append(value.description)
    }

    /// Appends the result of the given expression to the interpolation.
    @inlinable
    public mutating func appendInterpolation<T>(_ value: T) {
      _result.append(String(describing: value))
    }
  }

  /// Creates a string from the given string interpolation.
  @inlinable
  public init(stringInterpolation: StringInterpolation) {
    self = stringInterpolation._result
  }
}

// MARK: - Conversion from other types

extension String {
  /// Creates a string representing the given value.
  @inlinable
  public init<T>(_ value: T) where T : CustomStringConvertible {
    self = value.description
  }

  /// Creates a string representing the given value.
  @inlinable
  public init<T>(describing value: T) {
    if let customStringConvertible = value as? CustomStringConvertible {
      self = customStringConvertible.description
    } else {
      // Fallback for types that don't conform to CustomStringConvertible
      self = "\(value)"
    }
  }

  /// Creates a string from the given character.
  @inlinable
  public init(_ character: Character) {
    self._guts = _StringGuts(character)
  }
}

// MARK: - Character access

extension String {
  /// Accesses the character at the given position.
  @inlinable
  public subscript(position: String.Index) -> Character {
    get {
      return _guts[position]
    }
    set {
      _guts[position] = newValue
    }
  }

  /// Accesses the characters in the given range.
  @inlinable
  public subscript(bounds: Range<String.Index>) -> Substring {
    return Substring(_guts[bounds])
  }
}

// MARK: - String.Index

extension String {
  /// A position of a character or code unit in a string.
  @frozen
  public struct Index: Comparable {
    @usableFromInline
    internal var _position: Int

    @inlinable
    internal init(_position: Int) {
      self._position = _position
    }

    /// Returns a Boolean value indicating whether the first index is ordered
    /// before the second.
    @inlinable
    public static func < (lhs: String.Index, rhs: String.Index) -> Bool {
      return lhs._position < rhs._position
    }

    /// Returns a Boolean value indicating whether two indices are equal.
    @inlinable
    public static func == (lhs: String.Index, rhs: String.Index) -> Bool {
      return lhs._position == rhs._position
    }
  }

  /// The position of the first character in a nonempty string.
  @inlinable
  public var startIndex: String.Index {
    return String.Index(_position: 0)
  }

  /// The "past the end" position---that is, the position one greater than
  /// the last valid subscript argument.
  @inlinable
  public var endIndex: String.Index {
    return String.Index(_position: _guts.count)
  }

  /// Returns the position immediately after the given index.
  @inlinable
  public func index(after i: String.Index) -> String.Index {
    _precondition(i._position < _guts.count, "String index is out of bounds")
    return String.Index(_position: i._position + 1)
  }

  /// Returns the position immediately before the given index.
  @inlinable
  public func index(before i: String.Index) -> String.Index {
    _precondition(i._position > 0, "String index is out of bounds")
    return String.Index(_position: i._position - 1)
  }
}

// MARK: - Internal string storage

@usableFromInline
internal struct _StringGuts {
  @usableFromInline
  internal var _storage: _StringStorage

  @inlinable
  internal init() {
    _storage = _StringStorage()
  }

  @inlinable
  internal init(_ character: Character) {
    _storage = _StringStorage(character)
  }

  @inlinable
  internal var count: Int {
    return _storage.count
  }

  @inlinable
  internal mutating func append(_ other: _StringGuts) {
    _storage.append(other._storage)
  }

  @inlinable
  internal mutating func append(_ character: Character) {
    _storage.append(character)
  }

  @inlinable
  internal func isEqual(to other: _StringGuts) -> Bool {
    return _storage.isEqual(to: other._storage)
  }

  @inlinable
  internal func compare(with other: _StringGuts) -> Int {
    return _storage.compare(with: other._storage)
  }

  @inlinable
  internal func hash(into hasher: inout Hasher) {
    _storage.hash(into: &hasher)
  }

  @inlinable
  internal subscript(position: String.Index) -> Character {
    get { return _storage[position._position] }
    set { _storage[position._position] = newValue }
  }

  @inlinable
  internal subscript(bounds: Range<String.Index>) -> _StringGuts {
    return _StringGuts(_storage: _storage[bounds._position..<bounds.upperBound._position])
  }
}

// MARK: - Internal string storage implementation

@usableFromInline
internal struct _StringStorage {
  @usableFromInline
  internal var _buffer: [UInt8]

  @inlinable
  internal init() {
    _buffer = []
  }

  @inlinable
  internal init(_ character: Character) {
    _buffer = Array(character.utf8)
  }

  @inlinable
  internal init(_ buffer: [UInt8]) {
    _buffer = buffer
  }

  @inlinable
  internal var count: Int {
    // Simplified: return byte count (not character count)
    // In a real implementation, this would count Unicode grapheme clusters
    return _buffer.count
  }

  @inlinable
  internal mutating func append(_ other: _StringStorage) {
    _buffer.append(contentsOf: other._buffer)
  }

  @inlinable
  internal mutating func append(_ character: Character) {
    _buffer.append(contentsOf: character.utf8)
  }

  @inlinable
  internal func isEqual(to other: _StringStorage) -> Bool {
    return _buffer == other._buffer
  }

  @inlinable
  internal func compare(with other: _StringStorage) -> Int {
    // Simplified lexicographic comparison
    if _buffer < other._buffer { return -1 }
    if _buffer > other._buffer { return 1 }
    return 0
  }

  @inlinable
  internal func hash(into hasher: inout Hasher) {
    hasher.combine(_buffer)
  }

  @inlinable
  internal subscript(position: Int) -> Character {
    get {
      // Simplified: return Character from single byte
      // In a real implementation, this would handle Unicode properly
      return Character(UnicodeScalar(_buffer[position])!)
    }
    set {
      // Simplified: store single byte
      _buffer[position] = newValue.asciiValue ?? 0
    }
  }

  @inlinable
  internal subscript(bounds: Range<Int>) -> _StringStorage {
    return _StringStorage(Array(_buffer[bounds]))
  }
}