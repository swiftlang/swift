// RUN: not %target-swift-frontend %s -parse

public protocol Q_SequenceDefaultsType {
  typealias Element
  typealias Generator : GeneratorType
  func generate() -> Generator
}

extension Q_SequenceDefaultsType {
  public final func underestimateCount() -> Int { return 0 }
  public final func preprocessingPass<R>(body: (Self)->R) -> R? {
    return nil
  }

  /// Create a ContiguousArray containing the elements of `self`,
  /// in the same order.
  public final func copyToContiguousArray() -> ContiguousArray<Generator.Element> {
    let initialCapacity = underestimateCount()

    var result = _ContiguousArrayBuffer<Generator.Element>(
      count: initialCapacity, minimumCapacity: 0)

    var g = self.generate()
    while let x = g.next() {
      result += CollectionOfOne(x)
    }
    return ContiguousArray(result)
  }

  /// Initialize the storage at baseAddress with the contents of this
  /// sequence.
  public final func initializeRawMemory(
    baseAddress: UnsafeMutablePointer<Generator.Element>
  ) {
    var p = baseAddress
    var g = self.generate()
    while let element = g.next() {
      p.initialize(element)
      ++p
    }
  }

  public final static func _constrainElement(Generator.Element) {}
}

/// A type that can be iterated with a `for`\ ...\ `in` loop.
///
/// `SequenceType` makes no requirement on conforming types regarding
/// whether they will be destructively "consumed" by iteration.  To
/// ensure non-destructive iteration, constrain your *sequence* to
/// `CollectionType`.
public protocol Q_SequenceType : Q_SequenceDefaultsType {
  /// A type that provides the *sequence*\ 's iteration interface and
  /// encapsulates its iteration state.
  typealias Generator : GeneratorType

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  func generate() -> Generator

  /// Return a value less than or equal to the number of elements in
  /// self, **nondestructively**.
  ///
  /// Complexity: O(N)
  func underestimateCount() -> Int

  /// If `self` is multi-pass (i.e., a `CollectionType`), invoke the function
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

public extension GeneratorType {
  typealias Generator = Self
  
  public final func generate() -> Generator {
    return self
  }
}

public protocol Q_CollectionDefaultsType : Q_SequenceType {
  typealias Index : ForwardIndexType
  
  var startIndex: Index {get}
  var endIndex: Index {get}
}

public protocol Q_Indexable {
  typealias Index : ForwardIndexType
  typealias Element
  
  var startIndex: Index {get}
  var endIndex: Index {get}
  subscript(i: Index) -> Element {get}
}

extension Q_Indexable {
  typealias Generator = Q_IndexingGenerator<Self>
  public final func generate() -> Q_IndexingGenerator<Self> {
    return Q_IndexingGenerator(pos: self.startIndex, elements: self)
  }
}

extension Q_CollectionDefaultsType {
  public final func count() -> Index.Distance {
    return distance(startIndex, endIndex)
  }
  
  public final func underestimateCount() -> Int {
    let n = count().toIntMax()
    return n > IntMax(Int.max) ? Int.max : Int(n)
  }
  
  public final func preprocessingPass<R>(body: (Self)->R) -> R? {
    return body(self)
  }
}

public struct Q_IndexingGenerator<C: Q_Indexable> : GeneratorType {
  public typealias Element = C.Element
  var pos: C.Index
  let elements: C
  
  public mutating func next() -> Element? {
    if pos == elements.endIndex {
      return nil
    }
    let ret = elements[pos]
    ++pos
    return ret
  }
}

public protocol Q_CollectionType : Q_Indexable, Q_CollectionDefaultsType {
  func count() -> Index.Distance
  subscript(position: Index) -> Element {get}
}

extension Array : Q_CollectionType {
  public func copyToContiguousArray() -> ContiguousArray<Element> {
    return ContiguousArray(self~>_copyToNativeArrayBuffer())
  }
}

struct Boo : Q_CollectionType {
  let startIndex: Int = 0
  let endIndex: Int = 10

  func generate() -> Q_IndexingGenerator<Boo> {
    return Q_IndexingGenerator(pos: self.startIndex, elements: self)
  }

  typealias Element = String

  subscript(i: Int) -> String {
    return "Boo"
  }
}
