// RUN: not %target-swift-frontend %s -typecheck

public protocol Q_SequenceDefaults {
  typealias Element
  typealias Iterator : IteratorProtocol
  func makeIterator() -> Iterator
}

extension Q_SequenceDefaults {
  public final var underestimatedCount: Int { return 0 }
  public final func preprocessingPass<R>(body: (Self)->R) -> R? {
    return nil
  }

  /// Create a ContiguousArray containing the elements of `self`,
  /// in the same order.
  public final func copyToContiguousArray() -> ContiguousArray<Iterator.Element> {
    let initialCapacity = underestimatedCount

    var result = _ContiguousArrayBuffer<Iterator.Element>(
      count: initialCapacity, minimumCapacity: 0)

    var iter = self.makeIterator()
    while let x = iter.next() {
      result += CollectionOfOne(x)
    }
    return ContiguousArray(result)
  }

  /// Initialize the storage at baseAddress with the contents of this
  /// sequence.
  public final func initializeRawMemory(
    baseAddress: UnsafeMutablePointer<Iterator.Element>
  ) {
    var p = baseAddress
    var iter = self.makeIterator()
    while let element = iter.next() {
      p.initialize(to: element)
      p += 1
    }
  }

  public final static func _constrainElement(Iterator.Element) {}
}

/// A type that can be iterated with a `for`\ ...\ `in` loop.
///
/// `Sequence` makes no requirement on conforming types regarding
/// whether they will be destructively "consumed" by iteration.  To
/// ensure non-destructive iteration, constrain your *sequence* to
/// `Collection`.
public protocol Q_Sequence : Q_SequenceDefaults {
  /// A type that provides the *sequence*\ 's iteration interface and
  /// encapsulates its iteration state.
  typealias Iterator : IteratorProtocol

  func makeIterator() -> Iterator

  /// Return a value less than or equal to the number of elements in
  /// self, **nondestructively**.
  ///
  /// Complexity: O(N)
  var underestimatedCount: Int

  /// If `self` is multi-pass (i.e., a `Collection`), invoke the function
  /// on `self` and return its result.  Otherwise, return `nil`.
  func preprocessingPass<R>(body: (Self)->R) -> R?

  /// Create a ContiguousArray containing the elements of `self`,
  /// in the same order.
  func copyToContiguousArray() -> ContiguousArray<Element>

  /// Initialize the storage at baseAddress with the contents of this
  /// sequence.
  func initializeRawMemory(
    baseAddress: UnsafeMutablePointer<Element>
  )
  
  static func _constrainElement(Element)
}

public extension IteratorProtocol {
  typealias Iterator = Self
  
  public final func makeIterator() -> Iterator {
    return self
  }
}

public protocol Q_CollectionDefaults : Q_Sequence {
  typealias Index : ForwardIndex
  
  var startIndex: Index {get}
  var endIndex: Index {get}
}

public protocol Q_Indexable {
  typealias Index : ForwardIndex
  typealias Element
  
  var startIndex: Index {get}
  var endIndex: Index {get}
  subscript(i: Index) -> Element {get}
}

extension Q_Indexable {
  typealias Iterator = Q_IndexingIterator<Self>
  public final func makeIterator() -> Q_IndexingIterator<Self> {
    return Q_IndexingIterator(pos: self.startIndex, elements: self)
  }
}

extension Q_CollectionDefaults {
  public final func count() -> Index.Distance {
    return distance(startIndex, endIndex)
  }
  
  public final var underestimatedCount: Int {
    let n = count().toIntMax()
    return n > IntMax(Int.max) ? Int.max : Int(n)
  }
  
  public final func preprocessingPass<R>(body: (Self)->R) -> R? {
    return body(self)
  }
}

public struct Q_IndexingIterator<C: Q_Indexable> : IteratorProtocol {
  public typealias Element = C.Element
  var pos: C.Index
  let elements: C
  
  public mutating func next() -> Element? {
    if pos == elements.endIndex {
      return nil
    }
    let ret = elements[pos]
    pos += 1
    return ret
  }
}

public protocol Q_Collection : Q_Indexable, Q_CollectionDefaults {
  func count() -> Index.Distance
  subscript(position: Index) -> Element {get}
}

extension Array : Q_Collection {
  public func copyToContiguousArray() -> ContiguousArray<Element> {
    return ContiguousArray(self~>_copyToNativeArrayBuffer())
  }
}

struct Boo : Q_Collection {
  let startIndex: Int = 0
  let endIndex: Int = 10

  func makeIterator() -> Q_IndexingIterator<Boo> {
    return Q_IndexingIterator(pos: self.startIndex, elements: self)
  }

  typealias Element = String

  subscript(i: Int) -> String {
    return "Boo"
  }
}
