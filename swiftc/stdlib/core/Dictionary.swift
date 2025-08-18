//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A collection whose elements are key-value pairs.
///
/// A dictionary is a type of hash table, providing fast access to the values
/// based on their associated keys. Each entry in the table is known as a
/// key-value pair.
///
/// You create a new dictionary by using a dictionary literal. To create an
/// empty dictionary, use an empty dictionary literal (`[:]`).
///
///     var responseMessages = ["200": "OK",
///                             "403": "Access forbidden",
///                             "404": "File not found",
///                             "500": "Internal server error"]
///
///     var emptyDict: [String: String] = [:]
///
/// Any type that conforms to the `Hashable` protocol can be used as a
/// dictionary's `Key` type. A dictionary's `Value` type can be any type.
/// In a `[String: String]` dictionary, for example, the keys are of type
/// `String`, and the values are also of type `String`.
@frozen
public struct Dictionary<Key: Hashable, Value> {
  @usableFromInline
  internal var _storage: _DictionaryStorage<Key, Value>

  /// Creates an empty dictionary.
  @inlinable
  public init() {
    _storage = _DictionaryStorage()
  }

  /// Creates a dictionary with at least the given capacity.
  ///
  /// - Parameter minimumCapacity: The minimum number of key-value pairs that
  ///   the newly created dictionary should be able to store without
  ///   reallocating its storage.
  @inlinable
  public init(minimumCapacity: Int) {
    _storage = _DictionaryStorage(minimumCapacity: minimumCapacity)
  }

  /// Creates a new dictionary from the key-value pairs in the given sequence.
  ///
  /// You use this initializer to create a dictionary when you have a sequence
  /// of key-value tuples with unique keys. Passing a sequence with duplicate
  /// keys to this initializer results in a runtime error.
  ///
  /// The following example creates a new dictionary using an array of strings
  /// as the keys and the integers in a countable range as the values:
  ///
  ///     let digitWords = ["one", "two", "three", "four", "five"]
  ///     let wordToValue = Dictionary(uniqueKeysWithValues: zip(digitWords, 1...5))
  ///     print(wordToValue["three"]!)
  ///     // Prints "3"
  ///     print(wordToValue)
  ///     // Prints "["three": 3, "four": 4, "five": 5, "one": 1, "two": 2]"
  ///
  /// - Parameter keysAndValues: A sequence of key-value pairs to use for
  ///   the new dictionary. Every key in `keysAndValues` must be unique.
  /// - Returns: A new dictionary initialized with the elements of
  ///   `keysAndValues`.
  /// - Precondition: The sequence must not have duplicate keys.
  @inlinable
  public init<S>(uniqueKeysWithValues keysAndValues: S)
    where S : Sequence, S.Element == (Key, Value) {
    self.init()
    for (key, value) in keysAndValues {
      _precondition(self[key] == nil, "Dictionary literal contains duplicate keys")
      self[key] = value
    }
  }

  @inlinable
  internal init(_storage: _DictionaryStorage<Key, Value>) {
    self._storage = _storage
  }
}

// MARK: - Basic properties

extension Dictionary {
  /// The number of key-value pairs in the dictionary.
  ///
  /// - Complexity: O(1).
  @inlinable
  public var count: Int {
    return _storage.count
  }

  /// A Boolean value that indicates whether the dictionary is empty.
  ///
  /// Dictionaries are empty when created with an initializer or an empty
  /// dictionary literal.
  ///
  ///     var frequencies: [String: Int] = [:]
  ///     print(frequencies.isEmpty)
  ///     // Prints "true"
  ///
  /// - Complexity: O(1).
  @inlinable
  public var isEmpty: Bool {
    return count == 0
  }

  /// The total number of key-value pairs that the dictionary can contain
  /// without allocating new storage.
  @inlinable
  public var capacity: Int {
    return _storage.capacity
  }
}

// MARK: - Element access

extension Dictionary {
  /// Accesses the value associated with the given key for reading and writing.
  ///
  /// This *key-based* subscript returns the value for the given key if the key
  /// is found in the dictionary, or `nil` if the key is not found.
  ///
  /// The following example creates a new dictionary and prints the value of a
  /// key found in the dictionary (`"Coral"`) and a key not found in the
  /// dictionary (`"Cerise"`).
  ///
  ///     var hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///     print(hues["Coral"])
  ///     // Prints "Optional(16)"
  ///     print(hues["Cerise"])
  ///     // Prints "nil"
  ///
  /// When you assign a value for a key and that key already exists, the
  /// dictionary overwrites the existing value. If the dictionary doesn't
  /// contain the key, the key and value are added as a new key-value pair.
  ///
  /// Here, the value for the key `"Coral"` is updated from `16` to `18` and a
  /// new key-value pair is added for the key `"Cerise"`.
  ///
  ///     hues["Coral"] = 18
  ///     print(hues["Coral"])
  ///     // Prints "Optional(18)"
  ///
  ///     hues["Cerise"] = 330
  ///     print(hues["Cerise"])
  ///     // Prints "Optional(330)"
  ///
  /// If you assign `nil` as the value for the given key, the dictionary
  /// removes that key and its associated value.
  ///
  /// In the following example, the key-value pair for the key `"Aquamarine"`
  /// is removed from the dictionary by assigning `nil` to the key-based
  /// subscript.
  ///
  ///     hues["Aquamarine"] = nil
  ///     print(hues)
  ///     // Prints "["Coral": 18, "Heliotrope": 296, "Cerise": 330]"
  ///
  /// - Parameter key: The key to find in the dictionary.
  /// - Returns: The value associated with `key` if `key` is in the dictionary;
  ///   otherwise, `nil`.
  @inlinable
  public subscript(key: Key) -> Value? {
    get {
      return _storage.getValue(for: key)
    }
    set {
      _makeUnique()
      if let value = newValue {
        _storage.setValue(value, for: key)
      } else {
        _storage.removeValue(for: key)
      }
    }
  }

  /// Accesses the value with the given key, falling back to the given default
  /// value if the key isn't found.
  ///
  /// Use this subscript when you want either the value for a particular key
  /// or, when that key is not present in the dictionary, a default value.
  /// This example uses the subscript with a message to use in case an HTTP
  /// response code isn't recognized:
  ///
  ///     var responseMessages = [200: "OK",
  ///                             403: "Access forbidden",
  ///                             404: "File not found",
  ///                             500: "Internal server error"]
  ///
  ///     let httpResponseCodes = [200, 403, 301]
  ///     for code in httpResponseCodes {
  ///         let message = responseMessages[code, default: "Unknown response"]
  ///         print("Response \(code): \(message)")
  ///     }
  ///     // Prints "Response 200: OK"
  ///     // Prints "Response 403: Access forbidden"
  ///     // Prints "Response 301: Unknown response"
  ///
  /// When a dictionary's `Value` type has value semantics, you can use this
  /// subscript to perform in-place operations on values in the dictionary.
  /// The following example uses this subscript while counting the occurrences
  /// of each letter in a string:
  ///
  ///     let message = "Hello, Elle!"
  ///     var letterCounts: [Character: Int] = [:]
  ///     for letter in message {
  ///         letterCounts[letter, default: 0] += 1
  ///     }
  ///     // letterCounts == ["H": 1, "e": 4, "o": 1, " ": 1,
  ///     //                  ",": 1, "l": 4, "!": 1]
  ///
  /// When `letterCounts[letter, default: 0] += 1` is executed with a value of
  /// `letter` that isn't already a key in `letterCounts`, the specified
  /// default value (`0`) is returned from the subscript, incremented, and then
  /// added to the dictionary under that key.
  ///
  /// - Note: Do not use this subscript to modify dictionary values if the
  ///   dictionary's `Value` type is a class. In that case, the default value
  ///   and key are not written back to the dictionary after an operation.
  ///
  /// - Parameters:
  ///   - key: The key the look up in the dictionary.
  ///   - defaultValue: The default value to use if `key` doesn't exist in the
  ///     dictionary.
  /// - Returns: The value associated with `key` in the dictionary; otherwise,
  ///   `defaultValue`.
  @inlinable
  public subscript(key: Key, default defaultValue: @autoclosure () -> Value) -> Value {
    get {
      return _storage.getValue(for: key) ?? defaultValue()
    }
    set {
      _makeUnique()
      _storage.setValue(newValue, for: key)
    }
  }
}

// MARK: - Adding and removing elements

extension Dictionary {
  /// Updates the value stored in the dictionary for the given key, or adds a
  /// new key-value pair if the key does not exist.
  ///
  /// Use this method instead of key-based subscripting when you need to know
  /// whether the new value supplants the value of an existing key. If the
  /// value of an existing key is updated, `updateValue(_:forKey:)` returns
  /// the original value.
  ///
  ///     var hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///
  ///     if let oldValue = hues.updateValue(18, forKey: "Coral") {
  ///         print("The old value of \(oldValue) was replaced with a new one.")
  ///     }
  ///     // Prints "The old value of 16 was replaced with a new one."
  ///
  /// If the given key is not present in the dictionary, this method adds the
  /// key-value pair and returns `nil`.
  ///
  ///     if let oldValue = hues.updateValue(330, forKey: "Cerise") {
  ///         print("The old value of \(oldValue) was replaced with a new one.")
  ///     } else {
  ///         print("No value was found in the dictionary for that key.")
  ///     }
  ///     // Prints "No value was found in the dictionary for that key."
  ///
  /// - Parameters:
  ///   - value: The new value to add to the dictionary.
  ///   - key: The key to associate with `value`. If `key` already exists in
  ///     the dictionary, `value` replaces the existing associated value. If
  ///     `key` isn't already a key of the dictionary, the `key`-`value` pair
  ///     is added.
  /// - Returns: The value that was replaced, or `nil` if a new key-value pair
  ///   was added.
  @inlinable @discardableResult
  public mutating func updateValue(_ value: Value, forKey key: Key) -> Value? {
    _makeUnique()
    return _storage.updateValue(value, for: key)
  }

  /// Removes the given key and its associated value from the dictionary.
  ///
  /// If the key is found in the dictionary, this method returns the key's
  /// associated value. On removal, this method invalidates all indices with
  /// respect to the dictionary.
  ///
  ///     var hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///     if let value = hues.removeValue(forKey: "Coral") {
  ///         print("The value \(value) was removed.")
  ///     }
  ///     // Prints "The value 16 was removed."
  ///
  /// If the key isn't found in the dictionary, `removeValue(forKey:)` returns
  /// `nil`.
  ///
  ///     if let value = hues.removeValue(forKey: "Cerise") {
  ///         print("The value \(value) was removed.")
  ///     } else {
  ///         print("No value found for that key.")
  ///     }
  ///     // Prints "No value found for that key."
  ///
  /// - Parameter key: The key to remove along with its associated value.
  /// - Returns: The value that was removed, or `nil` if the key was not
  ///   present in the dictionary.
  ///
  /// - Complexity: O(1) on average, over many additions and removals.
  @inlinable @discardableResult
  public mutating func removeValue(forKey key: Key) -> Value? {
    _makeUnique()
    return _storage.removeValue(for: key)
  }

  /// Removes all key-value pairs from the dictionary.
  ///
  /// Calling this method invalidates all indices with respect to the
  /// dictionary.
  ///
  /// - Parameter keepCapacity: Whether the dictionary should keep its
  ///   underlying buffer. If you pass `true`, the operation preserves the
  ///   buffer capacity that the collection has, otherwise the underlying
  ///   buffer is released.  The default is `false`.
  ///
  /// - Complexity: O(*n*), where *n* is the number of key-value pairs in the
  ///   dictionary.
  @inlinable
  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    if !keepCapacity {
      _storage = _DictionaryStorage()
    } else {
      _makeUnique()
      _storage.removeAll()
    }
  }
}

// MARK: - Copy-on-write support

extension Dictionary {
  @inlinable
  internal mutating func _makeUnique() {
    if !isKnownUniquelyReferenced(&_storage._ref) {
      _storage = _storage._copy()
    }
  }
}

// MARK: - Keys and values collections

extension Dictionary {
  /// A collection containing just the keys of the dictionary.
  ///
  /// When iterated over, keys appear in this collection in the same order as
  /// they occur in the dictionary's key-value pairs. Each key in the keys
  /// collection has a unique value.
  ///
  ///     let countryCodes = ["BR": "Brazil", "GH": "Ghana", "JP": "Japan"]
  ///     print(countryCodes)
  ///     // Prints "["BR": "Brazil", "JP": "Japan", "GH": "Ghana"]"
  ///
  ///     for k in countryCodes.keys {
  ///         print(k)
  ///     }
  ///     // Prints "BR"
  ///     // Prints "JP"
  ///     // Prints "GH"
  @inlinable
  public var keys: Keys {
    return Keys(_dictionary: self)
  }

  /// A collection containing just the values of the dictionary.
  ///
  /// When iterated over, values appear in this collection in the same order as
  /// they occur in the dictionary's key-value pairs.
  ///
  ///     let countryCodes = ["BR": "Brazil", "GH": "Ghana", "JP": "Japan"]
  ///     print(countryCodes)
  ///     // Prints "["BR": "Brazil", "JP": "Japan", "GH": "Ghana"]"
  ///
  ///     for v in countryCodes.values {
  ///         print(v)
  ///     }
  ///     // Prints "Brazil"
  ///     // Prints "Japan"
  ///     // Prints "Ghana"
  @inlinable
  public var values: Values {
    return Values(_dictionary: self)
  }

  /// A view of a dictionary's keys.
  @frozen
  public struct Keys: Collection {
    @usableFromInline
    internal let _dictionary: Dictionary<Key, Value>

    @inlinable
    internal init(_dictionary: Dictionary<Key, Value>) {
      self._dictionary = _dictionary
    }

    public typealias Element = Key
    public typealias Index = Dictionary<Key, Value>.Index

    @inlinable
    public var startIndex: Index {
      return _dictionary.startIndex
    }

    @inlinable
    public var endIndex: Index {
      return _dictionary.endIndex
    }

    @inlinable
    public subscript(position: Index) -> Key {
      return _dictionary._storage.getKey(at: position._index)
    }

    @inlinable
    public func index(after i: Index) -> Index {
      return _dictionary.index(after: i)
    }
  }

  /// A view of a dictionary's values.
  @frozen
  public struct Values: MutableCollection {
    @usableFromInline
    internal var _dictionary: Dictionary<Key, Value>

    @inlinable
    internal init(_dictionary: Dictionary<Key, Value>) {
      self._dictionary = _dictionary
    }

    public typealias Element = Value
    public typealias Index = Dictionary<Key, Value>.Index

    @inlinable
    public var startIndex: Index {
      return _dictionary.startIndex
    }

    @inlinable
    public var endIndex: Index {
      return _dictionary.endIndex
    }

    @inlinable
    public subscript(position: Index) -> Value {
      get {
        return _dictionary._storage.getValue(at: position._index)
      }
      set {
        _dictionary._makeUnique()
        _dictionary._storage.setValue(newValue, at: position._index)
      }
    }

    @inlinable
    public func index(after i: Index) -> Index {
      return _dictionary.index(after: i)
    }
  }
}

// MARK: - ExpressibleByDictionaryLiteral

extension Dictionary: ExpressibleByDictionaryLiteral {
  /// Creates a dictionary initialized with a dictionary literal.
  ///
  /// Do not call this initializer directly. It is called by the compiler to
  /// handle dictionary literals. To create a dictionary using a dictionary
  /// literal, surround a comma-separated list of key-value pairs with square
  /// brackets:
  ///
  ///     let countryCodes = ["BR": "Brazil", "GH": "Ghana", "JP": "Japan"]
  ///     print(countryCodes)
  ///     // Prints "["BR": "Brazil", "JP": "Japan", "GH": "Ghana"]"
  ///
  /// - Parameter elements: The key-value pairs that will make up the new
  ///   dictionary. Each key in `elements` must be unique.
  @inlinable
  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.init(uniqueKeysWithValues: elements)
  }
}

// MARK: - Equatable conformance

extension Dictionary: Equatable where Value: Equatable {
  /// Returns a Boolean value indicating whether two dictionaries contain the
  /// same key-value pairs.
  @inlinable
  public static func == (lhs: Dictionary<Key, Value>, rhs: Dictionary<Key, Value>) -> Bool {
    return lhs._storage.isEqual(to: rhs._storage)
  }
}

// MARK: - Hashable conformance

extension Dictionary: Hashable where Value: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(count)
    for (key, value) in self {
      hasher.combine(key)
      hasher.combine(value)
    }
  }
}

// MARK: - CustomStringConvertible

extension Dictionary: CustomStringConvertible {
  /// A textual representation of the dictionary and its key-value pairs.
  public var description: String {
    if isEmpty {
      return "[:]"
    }
    
    var result = "["
    var first = true
    for (key, value) in self {
      if first {
        first = false
      } else {
        result += ", "
      }
      result += "\(key): \(value)"
    }
    result += "]"
    return result
  }
}

// MARK: - Internal dictionary storage

@usableFromInline
internal struct _DictionaryStorage<Key: Hashable, Value> {
  @usableFromInline
  internal var _ref: _DictionaryStorageRef<Key, Value>

  @inlinable
  internal init() {
    _ref = _DictionaryStorageRef()
  }

  @inlinable
  internal init(minimumCapacity: Int) {
    _ref = _DictionaryStorageRef(minimumCapacity: minimumCapacity)
  }

  @inlinable
  internal var count: Int {
    return _ref.count
  }

  @inlinable
  internal var capacity: Int {
    return _ref.capacity
  }

  @inlinable
  internal func getValue(for key: Key) -> Value? {
    return _ref.getValue(for: key)
  }

  @inlinable
  internal mutating func setValue(_ value: Value, for key: Key) {
    _ref.setValue(value, for: key)
  }

  @inlinable
  internal mutating func updateValue(_ value: Value, for key: Key) -> Value? {
    return _ref.updateValue(value, for: key)
  }

  @inlinable
  internal mutating func removeValue(for key: Key) -> Value? {
    return _ref.removeValue(for: key)
  }

  @inlinable
  internal mutating func removeAll() {
    _ref.removeAll()
  }

  @inlinable
  internal func getKey(at index: Int) -> Key {
    return _ref.getKey(at: index)
  }

  @inlinable
  internal func getValue(at index: Int) -> Value {
    return _ref.getValue(at: index)
  }

  @inlinable
  internal mutating func setValue(_ value: Value, at index: Int) {
    _ref.setValue(value, at: index)
  }

  @inlinable
  internal func isEqual<OtherValue>(to other: _DictionaryStorage<Key, OtherValue>) -> Bool where OtherValue: Equatable, Value == OtherValue {
    return _ref.isEqual(to: other._ref)
  }

  @inlinable
  internal func _copy() -> _DictionaryStorage<Key, Value> {
    return _DictionaryStorage(_ref: _ref._copy())
  }

  @inlinable
  internal init(_ref: _DictionaryStorageRef<Key, Value>) {
    self._ref = _ref
  }
}

// MARK: - Internal dictionary storage reference

@usableFromInline
internal class _DictionaryStorageRef<Key: Hashable, Value> {
  @usableFromInline
  internal var _buckets: [(Key, Value)?]
  
  @usableFromInline
  internal var _count: Int = 0

  @inlinable
  internal init() {
    _buckets = Array(repeating: nil, count: 8)
  }

  @inlinable
  internal init(minimumCapacity: Int) {
    let capacity = Swift.max(8, minimumCapacity.nextPowerOfTwo)
    _buckets = Array(repeating: nil, count: capacity)
  }

  @inlinable
  internal var count: Int {
    return _count
  }

  @inlinable
  internal var capacity: Int {
    return _buckets.count
  }

  @inlinable
  internal func getValue(for key: Key) -> Value? {
    let index = _findIndex(for: key)
    return _buckets[index]?.1
  }

  @inlinable
  internal func setValue(_ value: Value, for key: Key) {
    let index = _findIndex(for: key)
    if _buckets[index] == nil {
      _count += 1
      if _count > capacity * 3 / 4 {
        _resize()
        return setValue(value, for: key)
      }
    }
    _buckets[index] = (key, value)
  }

  @inlinable
  internal func updateValue(_ value: Value, for key: Key) -> Value? {
    let index = _findIndex(for: key)
    let oldValue = _buckets[index]?.1
    if _buckets[index] == nil {
      _count += 1
      if _count > capacity * 3 / 4 {
        _resize()
        return updateValue(value, for: key)
      }
    }
    _buckets[index] = (key, value)
    return oldValue
  }

  @inlinable
  internal func removeValue(for key: Key) -> Value? {
    let index = _findIndex(for: key)
    guard let (_, value) = _buckets[index] else { return nil }
    _buckets[index] = nil
    _count -= 1
    return value
  }

  @inlinable
  internal func removeAll() {
    _buckets = Array(repeating: nil, count: _buckets.count)
    _count = 0
  }

  @inlinable
  internal func getKey(at index: Int) -> Key {
    var currentIndex = 0
    for bucket in _buckets {
      if let (key, _) = bucket {
        if currentIndex == index {
          return key
        }
        currentIndex += 1
      }
    }
    _preconditionFailure("Dictionary index out of range")
  }

  @inlinable
  internal func getValue(at index: Int) -> Value {
    var currentIndex = 0
    for bucket in _buckets {
      if let (_, value) = bucket {
        if currentIndex == index {
          return value
        }
        currentIndex += 1
      }
    }
    _preconditionFailure("Dictionary index out of range")
  }

  @inlinable
  internal func setValue(_ value: Value, at index: Int) {
    var currentIndex = 0
    for i in _buckets.indices {
      if let (key, _) = _buckets[i] {
        if currentIndex == index {
          _buckets[i] = (key, value)
          return
        }
        currentIndex += 1
      }
    }
    _preconditionFailure("Dictionary index out of range")
  }

  @inlinable
  internal func isEqual<OtherValue>(to other: _DictionaryStorageRef<Key, OtherValue>) -> Bool where OtherValue: Equatable, Value == OtherValue {
    guard count == other.count else { return false }
    for bucket in _buckets {
      if let (key, value) = bucket {
        guard let otherValue = other.getValue(for: key), value == otherValue else {
          return false
        }
      }
    }
    return true
  }

  @inlinable
  internal func _copy() -> _DictionaryStorageRef<Key, Value> {
    let copy = _DictionaryStorageRef<Key, Value>()
    copy._buckets = self._buckets
    copy._count = self._count
    return copy
  }

  @inlinable
  internal func _findIndex(for key: Key) -> Int {
    let hash = key.hashValue
    var index = abs(hash) % capacity
    
    while let (existingKey, _) = _buckets[index] {
      if existingKey == key {
        return index
      }
      index = (index + 1) % capacity
    }
    return index
  }

  @inlinable
  internal func _resize() {
    let oldBuckets = _buckets
    let newCapacity = capacity * 2
    _buckets = Array(repeating: nil, count: newCapacity)
    _count = 0
    
    for bucket in oldBuckets {
      if let (key, value) = bucket {
        setValue(value, for: key)
      }
    }
  }
}

// MARK: - Helper extensions

extension Int {
  @inlinable
  internal var nextPowerOfTwo: Int {
    guard self > 1 else { return 1 }
    return 1 << (Int.bitWidth - (self - 1).leadingZeroBitCount)
  }
}