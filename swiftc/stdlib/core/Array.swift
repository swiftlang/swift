//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// An ordered, random-access collection.
///
/// Arrays are one of the most commonly used data types in an app. You use
/// arrays to organize your app's data. Specifically, you use the `Array` type
/// to hold elements of a single type, the array's `Element` type. An array
/// can store any kind of elements---from integers to strings to classes.
@frozen
public struct Array<Element> {
  @usableFromInline
  internal var _buffer: _ArrayBuffer<Element>

  /// Creates an empty array.
  @inlinable
  public init() {
    _buffer = _ArrayBuffer()
  }

  /// Creates an array containing the specified number of a single, repeated value.
  ///
  /// Here's an example of creating an array initialized with five strings
  /// containing the letter *Z*.
  ///
  ///     let fiveZs = Array(repeating: "Z", count: 5)
  ///     print(fiveZs)
  ///     // Prints "["Z", "Z", "Z", "Z", "Z"]"
  ///
  /// - Parameters:
  ///   - repeatedValue: The element to repeat.
  ///   - count: The number of times to repeat the value passed in the
  ///     `repeating` parameter. `count` must be zero or greater.
  @inlinable
  public init(repeating repeatedValue: Element, count: Int) {
    _precondition(count >= 0, "Array count must not be negative")
    _buffer = _ArrayBuffer(repeating: repeatedValue, count: count)
  }

  /// Creates an array containing the elements of a sequence.
  ///
  /// You can use this initializer to create an array from any other type that
  /// conforms to the `Sequence` protocol. For example, you might want to
  /// create an array with the integers from 1 through 7. Use this initializer
  /// around a range instead of typing all those numbers in an array literal.
  ///
  ///     let numbers = Array(1...7)
  ///     print(numbers)
  ///     // Prints "[1, 2, 3, 4, 5, 6, 7]"
  ///
  /// You can also use this initializer to convert a complex sequence or
  /// collection type back to an array. For example, the `keys` property of
  /// a dictionary isn't an array with a defined order, it's a collection of
  /// keys. If you need those keys sorted by their values, you can use this
  /// initializer around the sequence that results from sorting the keys.
  ///
  ///     let dict = ["Hearts": "♥", "Spades": "♠", "Diamonds": "♦", "Clubs": "♣"]
  ///     let sortedKeys = Array(dict.keys.sorted())
  ///     print(sortedKeys)
  ///     // Prints "["Clubs", "Diamonds", "Hearts", "Spades"]"
  ///
  /// - Parameter s: The sequence of elements to turn into an array.
  @inlinable
  public init<S>(_ s: S) where S : Sequence, S.Element == Element {
    _buffer = _ArrayBuffer(s)
  }

  @inlinable
  internal init(_buffer: _ArrayBuffer<Element>) {
    self._buffer = _buffer
  }
}

// MARK: - Basic properties

extension Array {
  /// The number of elements in the array.
  @inlinable
  public var count: Int {
    return _buffer.count
  }

  /// A Boolean value indicating whether the collection is empty.
  @inlinable
  public var isEmpty: Bool {
    return count == 0
  }

  /// The total number of elements that the array can contain without
  /// allocating new storage.
  @inlinable
  public var capacity: Int {
    return _buffer.capacity
  }
}

// MARK: - Element access

extension Array {
  /// Accesses the element at the specified position.
  ///
  /// The following example accesses an element of an array through its
  /// subscript to print its value:
  ///
  ///     var streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     print(streets[1])
  ///     // Prints "Bryant"
  ///
  /// You can subscript a collection with any valid index other than the
  /// collection's `endIndex`. The `endIndex` refers to the position one
  /// past the last element of a collection, so it doesn't correspond with an
  /// element.
  ///
  /// - Parameter index: The position of the element to access. `index` must be
  ///   greater than or equal to `startIndex` and less than `endIndex`.
  ///
  /// - Complexity: Reading an element from an array is O(1). Writing is O(1)
  ///   unless the array's storage is shared with another array, in which case
  ///   writing is O(*n*), where *n* is the length of the array.
  @inlinable
  public subscript(index: Int) -> Element {
    get {
      _checkIndex(index)
      return _buffer[index]
    }
    set {
      _checkIndex(index)
      _makeUnique()
      _buffer[index] = newValue
    }
  }

  /// Accesses a contiguous subrange of the collection's elements.
  ///
  /// The accessed slice uses the same indices for the same elements as the
  /// original collection uses. Always use the slice's `startIndex` property
  /// instead of assuming that its indices start at a particular value.
  ///
  /// This example demonstrates getting a slice of an array of strings, finding
  /// the index of one of the strings in the slice, and then using that index
  /// in the original array.
  ///
  ///     let streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     let streetsSlice = streets[2 ..< 4]
  ///     print(streetsSlice)
  ///     // Prints "["Channing", "Douglas"]"
  ///
  ///     let index = streetsSlice.firstIndex(of: "Channing")    // 2
  ///     print(streets[index!])
  ///     // Prints "Channing"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  ///
  /// - Complexity: O(1)
  @inlinable
  public subscript(bounds: Range<Int>) -> ArraySlice<Element> {
    get {
      _checkRange(bounds)
      return ArraySlice(_buffer: _buffer, bounds: bounds)
    }
    set {
      _checkRange(bounds)
      _makeUnique()
      _buffer.replaceSubrange(bounds, with: newValue)
    }
  }

  @inlinable
  internal func _checkIndex(_ index: Int) {
    _precondition(index >= 0 && index < count, "Array index out of range")
  }

  @inlinable
  internal func _checkRange(_ bounds: Range<Int>) {
    _precondition(bounds.lowerBound >= 0 && bounds.upperBound <= count, "Array range out of bounds")
  }
}

// MARK: - Adding elements

extension Array {
  /// Adds a new element at the end of the array.
  ///
  /// Use this method to append a single element to the end of a mutable array.
  ///
  ///     var numbers = [1, 2, 3, 4, 5]
  ///     numbers.append(100)
  ///     print(numbers)
  ///     // Prints "[1, 2, 3, 4, 5, 100]"
  ///
  /// Because arrays increase their allocated capacity using an exponential
  /// strategy, appending a single element to an array is an O(1) operation
  /// when averaged over many calls to the `append(_:)` method. When an array
  /// has additional capacity and is not sharing its storage with another
  /// instance, appending an element is O(1). When an array needs to
  /// reallocate storage before appending or its storage is shared with
  /// another copy, appending is O(*n*), where *n* is the length of the array.
  ///
  /// - Parameter newElement: The element to append to the array.
  ///
  /// - Complexity: O(1) on average, over many additions to the same array.
  @inlinable
  public mutating func append(_ newElement: Element) {
    _makeUnique()
    _buffer.append(newElement)
  }

  /// Adds the elements of a sequence to the end of the array.
  ///
  /// Use this method to append the elements of a sequence to the end of this
  /// array. This example appends the elements of a `Range<Int>` instance
  /// to an array of integers.
  ///
  ///     var numbers = [1, 2, 3, 4, 5]
  ///     numbers.append(contentsOf: 10...15)
  ///     print(numbers)
  ///     // Prints "[1, 2, 3, 4, 5, 10, 11, 12, 13, 14, 15]"
  ///
  /// - Parameter newElements: The elements to append to the array.
  ///
  /// - Complexity: O(*m*), where *m* is the length of `newElements`.
  @inlinable
  public mutating func append<S>(contentsOf newElements: S) where S : Sequence, S.Element == Element {
    _makeUnique()
    _buffer.append(contentsOf: newElements)
  }

  /// Inserts a new element at the specified position.
  ///
  /// The new element is inserted before the element currently at the specified
  /// index. If you pass the array's `endIndex` property as the `index`
  /// parameter, the new element is appended to the array.
  ///
  ///     var numbers = [1, 2, 3, 4, 5]
  ///     numbers.insert(100, at: 3)
  ///     print(numbers)
  ///     // Prints "[1, 2, 3, 100, 4, 5]"
  ///
  /// Calling this method may invalidate any existing indices for use with this
  /// collection.
  ///
  /// - Parameters:
  ///   - newElement: The new element to insert into the array.
  ///   - i: The position at which to insert the new element. `index` must be a
  ///     valid index of the array or equal to its `endIndex` property.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the array. If
  ///   `i == endIndex`, this method is equivalent to `append(_:)`.
  @inlinable
  public mutating func insert(_ newElement: Element, at i: Int) {
    _precondition(i >= 0 && i <= count, "Array index out of range")
    _makeUnique()
    _buffer.insert(newElement, at: i)
  }
}

// MARK: - Removing elements

extension Array {
  /// Removes and returns the element at the specified position.
  ///
  /// All the elements following the specified position are moved up to
  /// close the gap.
  ///
  ///     var measurements: [Double] = [1.1, 1.5, 2.9, 1.2, 1.5, 1.3, 1.2]
  ///     let removed = measurements.remove(at: 2)
  ///     print(measurements)
  ///     // Prints "[1.1, 1.5, 1.2, 1.5, 1.3, 1.2]"
  ///
  /// Calling this method may invalidate any existing indices for use with this
  /// collection.
  ///
  /// - Parameter index: The position of the element to remove. `index` must
  ///   be a valid index of the array.
  /// - Returns: The element at the specified index.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the array.
  @inlinable @discardableResult
  public mutating func remove(at index: Int) -> Element {
    _checkIndex(index)
    _makeUnique()
    return _buffer.remove(at: index)
  }

  /// Removes and returns the last element of the collection.
  ///
  /// Calling this method may invalidate all saved indices of this
  /// collection. Do not rely on a previously stored index value after
  /// altering a collection with any operation that can change its length.
  ///
  /// - Returns: The last element of the collection if the collection is not
  ///   empty; otherwise, `nil`.
  ///
  /// - Complexity: O(1)
  @inlinable @discardableResult
  public mutating func removeLast() -> Element {
    _precondition(!isEmpty, "Array is empty")
    _makeUnique()
    return _buffer.removeLast()
  }

  /// Removes and returns the last element of the collection.
  ///
  /// - Returns: The last element of the collection if the collection is not
  ///   empty; otherwise, `nil`.
  ///
  /// - Complexity: O(1)
  @inlinable @discardableResult
  public mutating func popLast() -> Element? {
    if isEmpty {
      return nil
    } else {
      return removeLast()
    }
  }

  /// Removes all elements from the array.
  ///
  /// - Parameter keepCapacity: Pass `true` to keep the existing capacity of
  ///   the array after removing its elements. The default value is `false`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the array.
  @inlinable
  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    if !keepCapacity {
      _buffer = _ArrayBuffer()
    } else {
      _makeUnique()
      _buffer.removeAll()
    }
  }
}

// MARK: - Copy-on-write support

extension Array {
  @inlinable
  internal mutating func _makeUnique() {
    if !isKnownUniquelyReferenced(&_buffer._storage) {
      _buffer = _buffer._copy()
    }
  }
}

// MARK: - ExpressibleByArrayLiteral

extension Array: ExpressibleByArrayLiteral {
  /// The type of the elements of an array literal.
  public typealias ArrayLiteralElement = Element

  /// Creates an array from the given array literal.
  ///
  /// Do not call this initializer directly. It is used by the compiler
  /// when you use an array literal. Instead, create a new array by using an
  /// array literal as its value. To do this, enclose a comma-separated list of
  /// values in square brackets.
  ///
  /// Here, an array of strings is created from an array literal holding
  /// only strings:
  ///
  ///     let ingredients = ["cocoa beans", "sugar", "cocoa butter", "salt"]
  ///
  /// - Parameter elements: A variadic list of elements of the new array.
  @inlinable
  public init(arrayLiteral elements: Element...) {
    self.init(elements)
  }
}

// MARK: - Equatable conformance

extension Array: Equatable where Element: Equatable {
  /// Returns a Boolean value indicating whether two arrays contain the same
  /// elements in the same order.
  ///
  /// You can check the equality of two arrays with the equal-to operator
  /// (`==`) or the not-equal-to operator (`!=`). The `==` operator is
  /// equivalent to calling the arrays' `elementsEqual(_:)` method.
  ///
  ///     let numbers = [1, 2, 3, 4, 5]
  ///     let otherNumbers = [1, 2, 3, 4, 5]
  ///
  ///     print(numbers == otherNumbers)
  ///     // Prints "true"
  ///     print(numbers != otherNumbers)
  ///     // Prints "false"
  ///
  /// - Parameters:
  ///   - lhs: An array to compare.
  ///   - rhs: Another array to compare.
  @inlinable
  public static func == (lhs: Array<Element>, rhs: Array<Element>) -> Bool {
    return lhs._buffer.elementsEqual(rhs._buffer)
  }
}

// MARK: - Hashable conformance

extension Array: Hashable where Element: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(count)
    for element in self {
      hasher.combine(element)
    }
  }
}

// MARK: - CustomStringConvertible

extension Array: CustomStringConvertible {
  /// A textual representation of the array and its elements.
  public var description: String {
    var result = "["
    var first = true
    for element in self {
      if first {
        first = false
      } else {
        result += ", "
      }
      result += String(describing: element)
    }
    result += "]"
    return result
  }
}

// MARK: - Internal array buffer

@usableFromInline
internal struct _ArrayBuffer<Element> {
  @usableFromInline
  internal var _storage: _ArrayStorage<Element>

  @inlinable
  internal init() {
    _storage = _ArrayStorage()
  }

  @inlinable
  internal init(repeating element: Element, count: Int) {
    _storage = _ArrayStorage(repeating: element, count: count)
  }

  @inlinable
  internal init<S>(_ sequence: S) where S: Sequence, S.Element == Element {
    _storage = _ArrayStorage(sequence)
  }

  @inlinable
  internal var count: Int {
    return _storage.count
  }

  @inlinable
  internal var capacity: Int {
    return _storage.capacity
  }

  @inlinable
  internal subscript(index: Int) -> Element {
    get { return _storage[index] }
    set { _storage[index] = newValue }
  }

  @inlinable
  internal mutating func append(_ element: Element) {
    _storage.append(element)
  }

  @inlinable
  internal mutating func append<S>(contentsOf sequence: S) where S: Sequence, S.Element == Element {
    _storage.append(contentsOf: sequence)
  }

  @inlinable
  internal mutating func insert(_ element: Element, at index: Int) {
    _storage.insert(element, at: index)
  }

  @inlinable
  internal mutating func remove(at index: Int) -> Element {
    return _storage.remove(at: index)
  }

  @inlinable
  internal mutating func removeLast() -> Element {
    return _storage.removeLast()
  }

  @inlinable
  internal mutating func removeAll() {
    _storage.removeAll()
  }

  @inlinable
  internal mutating func replaceSubrange<S>(_ bounds: Range<Int>, with newElements: S) where S: Sequence, S.Element == Element {
    _storage.replaceSubrange(bounds, with: newElements)
  }

  @inlinable
  internal func elementsEqual<Other>(_ other: _ArrayBuffer<Other>) -> Bool where Other: Equatable, Element == Other {
    return _storage.elementsEqual(other._storage)
  }

  @inlinable
  internal func _copy() -> _ArrayBuffer<Element> {
    return _ArrayBuffer(_storage: _storage._copy())
  }

  @inlinable
  internal init(_storage: _ArrayStorage<Element>) {
    self._storage = _storage
  }
}

// MARK: - Internal array storage

@usableFromInline
internal class _ArrayStorage<Element> {
  @usableFromInline
  internal var _elements: [Element]

  @inlinable
  internal init() {
    _elements = []
  }

  @inlinable
  internal init(repeating element: Element, count: Int) {
    _elements = Swift.Array(repeating: element, count: count)
  }

  @inlinable
  internal init<S>(_ sequence: S) where S: Sequence, S.Element == Element {
    _elements = Swift.Array(sequence)
  }

  @inlinable
  internal var count: Int {
    return _elements.count
  }

  @inlinable
  internal var capacity: Int {
    return _elements.capacity
  }

  @inlinable
  internal subscript(index: Int) -> Element {
    get { return _elements[index] }
    set { _elements[index] = newValue }
  }

  @inlinable
  internal func append(_ element: Element) {
    _elements.append(element)
  }

  @inlinable
  internal func append<S>(contentsOf sequence: S) where S: Sequence, S.Element == Element {
    _elements.append(contentsOf: sequence)
  }

  @inlinable
  internal func insert(_ element: Element, at index: Int) {
    _elements.insert(element, at: index)
  }

  @inlinable
  internal func remove(at index: Int) -> Element {
    return _elements.remove(at: index)
  }

  @inlinable
  internal func removeLast() -> Element {
    return _elements.removeLast()
  }

  @inlinable
  internal func removeAll() {
    _elements.removeAll()
  }

  @inlinable
  internal func replaceSubrange<S>(_ bounds: Range<Int>, with newElements: S) where S: Sequence, S.Element == Element {
    _elements.replaceSubrange(bounds, with: newElements)
  }

  @inlinable
  internal func elementsEqual<Other>(_ other: _ArrayStorage<Other>) -> Bool where Other: Equatable, Element == Other {
    return _elements.elementsEqual(other._elements)
  }

  @inlinable
  internal func _copy() -> _ArrayStorage<Element> {
    let copy = _ArrayStorage<Element>()
    copy._elements = self._elements
    return copy
  }
}