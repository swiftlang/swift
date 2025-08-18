//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A type that provides sequential, iterated access to its elements.
///
/// A sequence is a list of values that you can step through one at a time. The
/// most common way to iterate over the elements of a sequence is to use a
/// `for`-`in` loop:
///
///     let oneTwoThree = 1...3
///     for number in oneTwoThree {
///         print(number)
///     }
///     // Prints "1"
///     // Prints "2"  
///     // Prints "3"
///
/// While seemingly simple, this capability gives you access to a large number
/// of operations that you can perform on any sequence. As an example, to check
/// whether a sequence includes a particular value, you can test each value
/// sequentially until you've found a match or reached the end of the sequence.
///
/// Sequences provide access to their elements by creating an iterator, which
/// keeps track of its iteration process and returns one element at a time as
/// iteration progresses. All sequences also provide implementations for their
/// fundamental operations, such as `map(_:)`, `filter(_:)`, and `reduce(_:_:)`,
/// by using their iterator.
public protocol Sequence {
  /// A type representing the sequence's elements.
  associatedtype Element

  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  associatedtype Iterator: IteratorProtocol where Iterator.Element == Element

  /// Returns an iterator over the elements of this sequence.
  func makeIterator() -> Iterator
}

/// A type that supplies the values of a sequence one at a time.
///
/// The `IteratorProtocol` protocol is tightly linked with the `Sequence`
/// protocol. Sequences provide access to their elements by creating an
/// iterator, which keeps track of its iteration process and returns one
/// element at a time as iteration progresses.
///
/// Whenever you use a `for`-`in` loop with an array, set, or any other
/// collection or sequence, you're using that type's iterator. Swift uses a
/// sequence's or collection's iterator internally to present the loop body
/// with each element.
///
/// Using a sequence's iterator directly gives you access to the same elements
/// in the same order as iterating over that sequence using a `for`-`in` loop.
/// For example, you might typically use a `for`-`in` loop to print each of
/// the elements in an array:
///
///     let animals = ["Antelope", "Butterfly", "Camel", "Dolphin"]
///     for animal in animals {
///         print(animal)
///     }
///     // Prints "Antelope"
///     // Prints "Butterfly"
///     // Prints "Camel"
///     // Prints "Dolphin"
///
/// Behind the scenes, Swift uses the `animals` array's iterator to loop over
/// the contents of the array.
///
///     var animalIterator = animals.makeIterator()
///     while let animal = animalIterator.next() {
///         print(animal)
///     }
///     // Prints "Antelope"
///     // Prints "Butterfly"
///     // Prints "Camel"
///     // Prints "Dolphin"
public protocol IteratorProtocol {
  /// The type of element traversed by the iterator.
  associatedtype Element

  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Repeatedly calling this method returns, in order, all the elements of the
  /// underlying sequence. As soon as the sequence has run out of elements, all
  /// subsequent calls return `nil`.
  ///
  /// You must not call this method if any other copy of this iterator has been
  /// advanced with a call to its `next()` method.
  ///
  /// The following example shows how an iterator can be used explicitly to
  /// emulate a `for`-`in` loop. First, retrieve a sequence's iterator, and
  /// then call the iterator's `next()` method until it returns `nil`.
  ///
  ///     let numbers = [2, 3, 5, 7]
  ///     var numbersIterator = numbers.makeIterator()
  ///
  ///     while let num = numbersIterator.next() {
  ///         print(num)
  ///     }
  ///     // Prints "2"
  ///     // Prints "3"
  ///     // Prints "5"
  ///     // Prints "7"
  ///
  /// - Returns: The next element in the underlying sequence, if a next element
  ///   exists; otherwise, `nil`.
  mutating func next() -> Element?
}

// MARK: - Default implementations for Sequence

extension Sequence {
  /// Returns an array containing the results of mapping the given closure
  /// over the sequence's elements.
  ///
  /// In this example, `map` is used first to convert the names in the array
  /// to lowercase strings and then to count their characters.
  ///
  ///     let cast = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     let lowercaseNames = cast.map { $0.lowercased() }
  ///     // 'lowercaseNames' == ["vivien", "marlon", "kim", "karl"]
  ///     let letterCounts = cast.map { $0.count }
  ///     // 'letterCounts' == [6, 6, 3, 4]
  ///
  /// - Parameter transform: A mapping closure. `transform` accepts an
  ///   element of this sequence as its parameter and returns a transformed
  ///   value of the same or of a different type.
  /// - Returns: An array containing the transformed elements of this
  ///   sequence.
  public func map<T>(_ transform: (Element) throws -> T) rethrows -> [T] {
    var result: [T] = []
    for element in self {
      result.append(try transform(element))
    }
    return result
  }

  /// Returns an array containing, in order, the elements of the sequence
  /// that satisfy the given predicate.
  ///
  /// In this example, `filter(_:)` is used to include only names shorter than
  /// five characters.
  ///
  ///     let cast = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     let shortNames = cast.filter { $0.count < 5 }
  ///     print(shortNames)
  ///     // Prints "["Kim", "Karl"]"
  ///
  /// - Parameter isIncluded: A closure that takes an element of the
  ///   sequence as its argument and returns a Boolean value indicating
  ///   whether the element should be included in the returned array.
  /// - Returns: An array of the elements that `isIncluded` allowed.
  public func filter(_ isIncluded: (Element) throws -> Bool) rethrows -> [Element] {
    var result: [Element] = []
    for element in self {
      if try isIncluded(element) {
        result.append(element)
      }
    }
    return result
  }

  /// Returns the result of combining the elements of the sequence using the
  /// given closure.
  ///
  /// Use the `reduce(_:_:)` method to produce a single value from the elements
  /// of an entire sequence. For example, you can use this method on an array
  /// of numbers to find their sum or product.
  ///
  /// The `nextPartialResult` closure is called sequentially with an
  /// accumulating value initialized to `initialResult` and each element of
  /// the sequence. This example shows how to find the sum of an array of
  /// numbers.
  ///
  ///     let numbers = [1, 2, 3, 4]
  ///     let numberSum = numbers.reduce(0, { x, y in
  ///         x + y
  ///     })
  ///     // numberSum == 10
  ///
  /// When `numbers.reduce(_:_:)` is called, the following steps occur:
  ///
  /// 1. The `nextPartialResult` closure is called with `initialResult`---`0`
  ///    in this case---and the first element of `numbers`, returning the sum:
  ///    `1`.
  /// 2. The closure is called again repeatedly with the previous call's return
  ///    value and each element of the sequence.
  /// 3. When the sequence is exhausted, the last value returned from the
  ///    closure is returned to the caller.
  ///
  /// If the sequence has no elements, `nextPartialResult` is never executed
  /// and `initialResult` is the result of the call to `reduce(_:_:)`.
  ///
  /// - Parameters:
  ///   - initialResult: The value to use as the initial accumulating value.
  ///     `initialResult` is passed to `nextPartialResult` the first time the
  ///     closure is executed.
  ///   - nextPartialResult: A closure that combines an accumulating value and
  ///     an element of the sequence into a new accumulating value, to be used
  ///     in the next call of the `nextPartialResult` closure or returned to
  ///     the caller.
  /// - Returns: The final accumulated value. If the sequence has no elements,
  ///   the result is `initialResult`.
  public func reduce<Result>(_ initialResult: Result, _ nextPartialResult: (Result, Element) throws -> Result) rethrows -> Result {
    var accumulator = initialResult
    for element in self {
      accumulator = try nextPartialResult(accumulator, element)
    }
    return accumulator
  }

  /// Returns the first element of the sequence that satisfies the given
  /// predicate.
  ///
  /// The following example uses the `first(where:)` method to find the first
  /// negative number in an array of integers:
  ///
  ///     let numbers = [3, 7, 4, -2, 9, -6, 10, 1]
  ///     if let firstNegative = numbers.first(where: { $0 < 0 }) {
  ///         print("The first negative number is \(firstNegative).")
  ///     }
  ///     // Prints "The first negative number is -2."
  ///
  /// - Parameter predicate: A closure that takes an element of the sequence as
  ///   its argument and returns a Boolean value indicating whether the element
  ///   is a match.
  /// - Returns: The first element of the sequence that satisfies `predicate`,
  ///   or `nil` if there is no element that satisfies `predicate`.
  public func first(where predicate: (Element) throws -> Bool) rethrows -> Element? {
    for element in self {
      if try predicate(element) {
        return element
      }
    }
    return nil
  }

  /// Returns a Boolean value indicating whether the sequence contains an
  /// element that satisfies the given predicate.
  ///
  /// You can use the predicate to check for an element of a type that
  /// doesn't conform to the `Equatable` protocol, such as the
  /// `HTTPResponse` enumeration in this example.
  ///
  ///     enum HTTPResponse {
  ///         case ok
  ///         case error(Int)
  ///     }
  ///
  ///     let responses: [HTTPResponse] = [.ok, .ok, .error(404), .error(200)]
  ///     let hasError = responses.contains { element in
  ///         if case .error = element {
  ///             return true
  ///         } else {
  ///             return false
  ///         }
  ///     }
  ///     // 'hasError' == true
  ///
  /// Alternatively, a predicate can be satisfied by a range of `Equatable`
  /// elements or a general condition. This example shows how you can check an
  /// array for an expense greater than $100.
  ///
  ///     let expenses = [21.37, 55.21, 9.32, 10.18, 198.54, 41.19, 78.20]
  ///     let hasBigPurchase = expenses.contains { $0 > 100 }
  ///     // 'hasBigPurchase' == true
  ///
  /// - Parameter predicate: A closure that takes an element of the sequence as
  ///   its argument and returns a Boolean value that indicates whether the
  ///   passed element represents a match.
  /// - Returns: `true` if the sequence contains an element that satisfies
  ///   `predicate`; otherwise, `false`.
  public func contains(where predicate: (Element) throws -> Bool) rethrows -> Bool {
    for element in self {
      if try predicate(element) {
        return true
      }
    }
    return false
  }

  /// Returns a Boolean value indicating whether every element of a sequence
  /// satisfies a given predicate.
  ///
  /// The following code uses this method to test whether all the names in an
  /// array have at least five characters:
  ///
  ///     let names = ["Sofia", "Camilla", "Martina", "Mateo", "Nicolás"]
  ///     let allHaveAtLeastFive = names.allSatisfy({ $0.count >= 5 })
  ///     // allHaveAtLeastFive == true
  ///
  /// - Parameter predicate: A closure that takes an element of the sequence as
  ///   its argument and returns a Boolean value that indicates whether the
  ///   passed element satisfies a condition.
  /// - Returns: `true` if the sequence contains only elements that satisfy
  ///   `predicate`; otherwise, `false`.
  public func allSatisfy(_ predicate: (Element) throws -> Bool) rethrows -> Bool {
    for element in self {
      if try !predicate(element) {
        return false
      }
    }
    return true
  }

  /// Returns an array containing the non-`nil` results of calling the given
  /// transformation with each element of this sequence.
  ///
  /// Use this method to receive an array of non-optional values when your
  /// transformation produces an optional value.
  ///
  /// In this example, note the difference in the result of using `map` and
  /// `compactMap` with a transformation that returns an optional `Int` value.
  ///
  ///     let possibleNumbers = ["1", "2", "three", "///4///", "5"]
  ///
  ///     let mapped: [Int?] = possibleNumbers.map { str in Int(str) }
  ///     // [1, 2, nil, nil, 5]
  ///
  ///     let compactMapped: [Int] = possibleNumbers.compactMap { str in Int(str) }
  ///     // [1, 2, 5]
  ///
  /// - Parameter transform: A closure that accepts an element of this
  ///   sequence as its argument and returns an optional value.
  /// - Returns: An array of the non-`nil` results of calling `transform`
  ///   with each element of the sequence.
  public func compactMap<ElementOfResult>(_ transform: (Element) throws -> ElementOfResult?) rethrows -> [ElementOfResult] {
    var result: [ElementOfResult] = []
    for element in self {
      if let transformed = try transform(element) {
        result.append(transformed)
      }
    }
    return result
  }

  /// Returns an array containing the concatenated results of calling the
  /// given transformation with each element of this sequence.
  ///
  /// Use this method to receive a single-level collection when your
  /// transformation produces a sequence or collection for each element.
  ///
  /// In this example, note the difference between using `map` and `flatMap`:
  ///
  ///     let numbers = [1, 2, 3, 4]
  ///
  ///     let mapped = numbers.map { Array(repeating: $0, count: $0) }
  ///     // [[1], [2, 2], [3, 3, 3], [4, 4, 4, 4]]
  ///
  ///     let flatMapped = numbers.flatMap { Array(repeating: $0, count: $0) }
  ///     // [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
  ///
  /// - Parameter transform: A closure that accepts an element of this
  ///   sequence as its argument and returns a sequence or collection.
  /// - Returns: The resulting flattened array.
  public func flatMap<SegmentOfResult>(_ transform: (Element) throws -> SegmentOfResult) rethrows -> [SegmentOfResult.Element] where SegmentOfResult : Sequence {
    var result: [SegmentOfResult.Element] = []
    for element in self {
      let segment = try transform(element)
      result.append(contentsOf: segment)
    }
    return result
  }

  /// Returns the minimum element in the sequence, using the given predicate as
  /// the comparison between elements.
  ///
  /// This example shows how to use the `min(by:)` method on a dictionary's
  /// key-value pairs to find the pair with the lowest value.
  ///
  ///     let hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///     let leastHue = hues.min { a, b in a.value < b.value }
  ///     print(leastHue)
  ///     // Prints "Optional(("Coral", 16))"
  ///
  /// - Parameter areInIncreasingOrder: A predicate that returns `true` if its
  ///   first argument should be ordered before its second argument;
  ///   otherwise, `false`.
  /// - Returns: The sequence's minimum element, according to
  ///   `areInIncreasingOrder`. If the sequence has no elements, returns `nil`.
  public func min(by areInIncreasingOrder: (Element, Element) throws -> Bool) rethrows -> Element? {
    var iterator = makeIterator()
    guard var minimum = iterator.next() else { return nil }
    
    while let element = iterator.next() {
      if try areInIncreasingOrder(element, minimum) {
        minimum = element
      }
    }
    return minimum
  }

  /// Returns the maximum element in the sequence, using the given predicate as
  /// the comparison between elements.
  ///
  /// This example shows how to use the `max(by:)` method on a dictionary's
  /// key-value pairs to find the pair with the highest value.
  ///
  ///     let hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///     let greatestHue = hues.max { a, b in a.value < b.value }
  ///     print(greatestHue)
  ///     // Prints "Optional(("Heliotrope", 296))"
  ///
  /// - Parameter areInIncreasingOrder: A predicate that returns `true` if its
  ///   first argument should be ordered before its second argument;
  ///   otherwise, `false`.
  /// - Returns: The sequence's maximum element, according to
  ///   `areInIncreasingOrder`. If the sequence has no elements, returns `nil`.
  public func max(by areInIncreasingOrder: (Element, Element) throws -> Bool) rethrows -> Element? {
    var iterator = makeIterator()
    guard var maximum = iterator.next() else { return nil }
    
    while let element = iterator.next() {
      if try areInIncreasingOrder(maximum, element) {
        maximum = element
      }
    }
    return maximum
  }

  /// Returns a Boolean value indicating whether the sequence and another
  /// sequence contain equivalent elements in the same order, using the given
  /// predicate as the equivalence test.
  ///
  /// At least one of the sequences must be finite.
  ///
  /// This example uses the `elementsEqual(_:by:)` method to test whether one
  /// countable range begins with the elements of another countable range.
  ///
  ///     let a = 1...3
  ///     let b = 1...10
  ///
  ///     print(b.elementsEqual(a) { $0 == $1 })
  ///     // Prints "false"
  ///
  ///     print(a.elementsEqual(b) { $0 == $1 })
  ///     // Prints "false"
  ///
  /// - Parameters:
  ///   - other: A sequence to compare to this sequence.
  ///   - areEquivalent: A predicate that returns `true` if its two arguments
  ///     are equivalent; otherwise, `false`.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  public func elementsEqual<OtherSequence>(_ other: OtherSequence, by areEquivalent: (Element, OtherSequence.Element) throws -> Bool) rethrows -> Bool where OtherSequence : Sequence {
    var iterator1 = self.makeIterator()
    var iterator2 = other.makeIterator()
    
    while true {
      let element1 = iterator1.next()
      let element2 = iterator2.next()
      
      switch (element1, element2) {
      case (nil, nil):
        return true
      case (let e1?, let e2?):
        if try !areEquivalent(e1, e2) {
          return false
        }
      default:
        return false
      }
    }
  }
}

// MARK: - Equatable element extensions

extension Sequence where Element: Equatable {
  /// Returns a Boolean value indicating whether the sequence contains the
  /// given element.
  ///
  /// This example checks to see whether a favorite actor is in an array
  /// storing a movie's cast.
  ///
  ///     let cast = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     print(cast.contains("Marlon"))
  ///     // Prints "true"
  ///     print(cast.contains("James"))
  ///     // Prints "false"
  ///
  /// - Parameter element: The element to find in the sequence.
  /// - Returns: `true` if the element was found in the sequence; otherwise,
  ///   `false`.
  public func contains(_ element: Element) -> Bool {
    return contains { $0 == element }
  }

  /// Returns a Boolean value indicating whether this sequence and another
  /// sequence contain the same elements in the same order.
  ///
  /// At least one of the sequences must be finite.
  ///
  /// This example tests whether one countable range begins with the elements
  /// of another countable range.
  ///
  ///     let a = 1...3
  ///     let b = 1...10
  ///
  ///     print(a.elementsEqual(b))
  ///     // Prints "false"
  ///
  /// - Parameter other: A sequence to compare to this sequence.
  /// - Returns: `true` if this sequence and `other` contain the same elements
  ///   in the same order.
  public func elementsEqual<OtherSequence>(_ other: OtherSequence) -> Bool where OtherSequence : Sequence, OtherSequence.Element == Element {
    return elementsEqual(other, by: ==)
  }
}

// MARK: - Comparable element extensions

extension Sequence where Element: Comparable {
  /// Returns the minimum element in the sequence.
  ///
  /// This example finds the smallest value in an array of height measurements.
  ///
  ///     let heights = [67.5, 65.7, 64.3, 61.1, 58.5, 60.3, 64.9]
  ///     let lowestHeight = heights.min()
  ///     print(lowestHeight)
  ///     // Prints "Optional(58.5)"
  ///
  /// - Returns: The sequence's minimum element. If the sequence has no
  ///   elements, returns `nil`.
  public func min() -> Element? {
    return min(by: <)
  }

  /// Returns the maximum element in the sequence.
  ///
  /// This example finds the largest value in an array of height measurements.
  ///
  ///     let heights = [67.5, 65.7, 64.3, 61.1, 58.5, 60.3, 64.9]
  ///     let greatestHeight = heights.max()
  ///     print(greatestHeight)
  ///     // Prints "Optional(67.5)"
  ///
  /// - Returns: The sequence's maximum element. If the sequence has no
  ///   elements, returns `nil`.
  public func max() -> Element? {
    return max(by: <)
  }

  /// Returns the elements of the sequence, sorted.
  ///
  /// You can sort any sequence of elements that conform to the `Comparable`
  /// protocol by calling this method. Elements are sorted in ascending order.
  ///
  /// The sorting algorithm is not guaranteed to be stable. A stable sort
  /// preserves the relative order of elements that compare as equal.
  ///
  /// Here's an example of sorting a list of students' names. Strings in Swift
  /// conform to the `Comparable` protocol, so the names are sorted in
  /// ascending alphabetical order according to the less-than operator (`<`).
  ///
  ///     let students: Set = ["Kofi", "Abena", "Peter", "Kweku", "Akosua"]
  ///     let sortedStudents = students.sorted()
  ///     print(sortedStudents)
  ///     // Prints "["Abena", "Akosua", "Kofi", "Kweku", "Peter"]"
  ///
  /// To sort the elements of your sequence in descending order, pass the
  /// greater-than operator (`>`) to the `sorted(by:)` method.
  ///
  ///     let descendingStudents = students.sorted(by: >)
  ///     print(descendingStudents)
  ///     // Prints "["Peter", "Kweku", "Kofi", "Akosua", "Abena"]"
  ///
  /// - Returns: A sorted array of the sequence's elements.
  public func sorted() -> [Element] {
    return sorted(by: <)
  }
}

// MARK: - Sorting

extension Sequence {
  /// Returns the elements of the sequence, sorted using the given predicate as
  /// the comparison between elements.
  ///
  /// When you want to sort a sequence of elements that don't conform to the
  /// `Comparable` protocol, pass a predicate to this method that returns `true`
  /// when the first element should be ordered before the second. The elements
  /// of the resulting array are ordered according to the given predicate.
  ///
  /// The predicate must be a *strict weak ordering* over the elements. That
  /// is, for any elements `a`, `b`, and `c`:
  ///
  /// - `areInIncreasingOrder(a, a)` is always `false`. (Irreflexivity)
  /// - If `areInIncreasingOrder(a, b)` and `areInIncreasingOrder(b, c)` are
  ///   both `true`, then `areInIncreasingOrder(a, c)` is also
  ///   `true`. (Transitive comparability)
  /// - Two elements are *incomparable* if neither is ordered before the other
  ///   according to the predicate. If `a` and `b` are incomparable, and `b`
  ///   and `c` are incomparable, then `a` and `c` are also incomparable.
  ///   (Transitive incomparability)
  ///
  /// The sorting algorithm is not guaranteed to be stable. A stable sort
  /// preserves the relative order of elements for which
  /// `areInIncreasingOrder` does not establish an order.
  ///
  /// In the following example, the predicate provides an ordering for an array
  /// of a custom `HTTPResponse` type. The predicate orders errors before
  /// successes and sorts the error responses by their error code.
  ///
  ///     enum HTTPResponse {
  ///         case ok
  ///         case error(Int)
  ///     }
  ///
  ///     let responses: [HTTPResponse] = [.error(500), .ok, .ok, .error(404), .error(403)]
  ///     let sortedResponses = responses.sorted {
  ///         switch ($0, $1) {
  ///         // Order errors by code
  ///         case let (.error(aCode), .error(bCode)):
  ///             return aCode < bCode
  ///
  ///         // All errors come before successes
  ///         case (.error, .ok): return true
  ///         case (.ok, .error): return false
  ///
  ///         // Success responses are equivalent
  ///         case (.ok, .ok): return false
  ///         }
  ///     }
  ///     print(sortedResponses)
  ///     // Prints "[.error(403), .error(404), .error(500), .ok, .ok]"
  ///
  /// You also use this method to sort elements that conform to the
  /// `Comparable` protocol in descending order. To sort your sequence in
  /// descending order, pass the greater-than operator (`>`) as the
  /// `areInIncreasingOrder` parameter.
  ///
  ///     let students: Set = ["Kofi", "Abena", "Peter", "Kweku", "Akosua"]
  ///     let descendingStudents = students.sorted(by: >)
  ///     print(descendingStudents)
  ///     // Prints "["Peter", "Kweku", "Kofi", "Akosua", "Abena"]"
  ///
  /// Calling the related `sorted()` method is equivalent to calling this
  /// method and passing the less-than operator (`<`) as the predicate.
  ///
  ///     print(students.sorted())
  ///     // Prints "["Abena", "Akosua", "Kofi", "Kweku", "Peter"]"
  ///     print(students.sorted(by: <))
  ///     // Prints "["Abena", "Akosua", "Kofi", "Kweku", "Peter"]"
  ///
  /// - Parameter areInIncreasingOrder: A predicate that returns `true` if its
  ///   first argument should be ordered before its second argument;
  ///   otherwise, `false`.
  /// - Returns: A sorted array of the sequence's elements.
  public func sorted(by areInIncreasingOrder: (Element, Element) throws -> Bool) rethrows -> [Element] {
    var array = Array(self)
    try array.sort(by: areInIncreasingOrder)
    return array
  }
}

// MARK: - Convenience initializers

extension Array {
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
  /// - Parameter s: The sequence of elements to turn into an array.
  public init<S>(_ s: S) where S : Sequence, S.Element == Element {
    self.init()
    for element in s {
      self.append(element)
    }
  }
}