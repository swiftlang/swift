// Contains experimental implementations of FixedArray and BufferView

@frozen
public struct FixedArray<T> : ~Copyable {
  @usableFromInline
  let buffer: UnsafeMutablePointer<T>

  public let capacity: Int

  @usableFromInline
  internal var _count: Int

  // Workaround until FixedArray itself can conform to ExpressibleByArrayLiteral
  @frozen
  public struct _Literal: ExpressibleByArrayLiteral {
    @usableFromInline
    let array: [T]

    @_transparent
    @_alwaysEmitIntoClient
    public init(arrayLiteral elements: T...) {
      self.array = elements
    }
  }

  @_transparent
  @_alwaysEmitIntoClient
  public init(capacity: Int) {
    self.capacity = capacity
    self.buffer = _allocateVector(elementType: T.self, capacity: capacity)
    self._count = 0
  }

  @_transparent
  @_alwaysEmitIntoClient
  public init(elements: [T]) {
    let count = elements.count
    self._count = count
    self.capacity = count
    self.buffer = elements.createVector()
  }

  @_transparent
  @_alwaysEmitIntoClient
  public init(repeating repeatedValue: T, count: Int) {
    self.capacity = count
    self.buffer = _allocateVector(elementType: T.self, capacity: capacity)
    self._count = count
    for i in 0..<count {
      (buffer + i).initialize(to: repeatedValue)
    }
  }

  @_alwaysEmitIntoClient
  public var count: Int { _count }

  // TODO: make this a computed property, once rdar://119305513 is fixed
  @_alwaysEmitIntoClient
  public func view() -> BufferView<T> {
    BufferView(baseAddress: buffer, count: count)
  }

  public subscript(position: Int) -> T {
    @_alwaysEmitIntoClient
    get {
      precondition(position >= 0 && position < count)
      return buffer[position]
    }

    @_alwaysEmitIntoClient
    set {
      precondition(position >= 0 && position < count)
      buffer[position] = newValue
    }
  }


  // Will be used once it's possible to let FixedArray conform to Collection.
  public var startIndex: Int { 0 }
  public var endIndex: Int { count }
  public func index(after i: Int) -> Int { return i + 1 }

  @_alwaysEmitIntoClient
  public mutating func append(_ element: T) {
    precondition(count < capacity, "capacity overflow")
    (buffer + count).initialize(to: element)
    _count &+= 1
  }

  @_alwaysEmitIntoClient
  deinit {
    buffer.deinitialize(count: count)
  }
}

// Syntactic sugar for creating a FixedArray literal
prefix operator ^

@_transparent
@_alwaysEmitIntoClient
public prefix func ^<T>(_ literal: FixedArray<T>._Literal) -> FixedArray<T> {
  return FixedArray<T>(elements: literal.array)
}

// Syntactic sugar for getting a BufferView to a FixedArray
prefix operator *

@_transparent
@_alwaysEmitIntoClient
public prefix func *<T>(array: borrowing FixedArray<T>) -> BufferView<T> {
  return array.view()
}

extension Array {
  @_transparent
  @_alwaysEmitIntoClient
  public func createVector() -> UnsafeMutablePointer<Element> {
    let count = self.count
    let vector = _allocateVector(elementType: Element.self, capacity: count)
    copyInto(vectorBuffer: vector)
    return vector
  }

  @_alwaysEmitIntoClient
  @_semantics("array.copy_into_vector")
  internal func copyInto(vectorBuffer: UnsafeMutablePointer<Element>) {
    withUnsafeBufferPointer {
      vectorBuffer.initialize(from: $0.baseAddress!, count: $0.count)
    }
  }
}

@frozen
public struct BufferViewIndex<Element> : Equatable {
  public typealias Pointer = UnsafePointer<Element>

  @usableFromInline
  let _pointer: Pointer

  @_alwaysEmitIntoClient
  public init(pointer: Pointer) {
    _pointer = pointer
  }
}

extension BufferViewIndex : Strideable {
  public typealias Stride = Int

  @_alwaysEmitIntoClient
  public func distance(to other: BufferViewIndex) -> Int {
    _pointer.distance(to: other._pointer)
  }

  @_alwaysEmitIntoClient
  public func advanced(by n: Int) -> BufferViewIndex {
    .init(pointer: _pointer.advanced(by: n))
  }
}

extension BufferViewIndex : Comparable {
  @_alwaysEmitIntoClient
  public static func < (lhs: BufferViewIndex, rhs: BufferViewIndex) -> Bool {
    lhs._pointer < rhs._pointer
  }
}

@frozen
public struct BufferView<Element>: Collection {
  // BufferView is self-slicing. The implementation is omitted for our purpose.
  public typealias SubSequence = BufferView<Element>

  public typealias Index = BufferViewIndex<Element>
  public typealias Pointer = Index.Pointer

  @usableFromInline
  let start: Index

  public let count: Int

  // Initialization is internal because a user-specified count is unsafe.
  @_alwaysEmitIntoClient
  init(start index: Index, count: Int) {
    precondition(count >= 0, "Count must not be negative")
    self.start = index
    self.count = count
  }

  @_alwaysEmitIntoClient
  init(baseAddress: Pointer, count: Int) {
    self.init(start: .init(pointer: baseAddress), count: count)
  }

  // An unsafe public API serves as the low-level entry point for
  // BufferView creation.
  @_alwaysEmitIntoClient
  public static func withTemporaryView<ResultType>(
    unsafeBaseAddress: Pointer, unsafeCount: Int,
    _ body: (borrowing BufferView<Element>) throws -> ResultType
  ) rethrows -> ResultType {
    try body(BufferView<Element>(baseAddress: unsafeBaseAddress,
                                 count: unsafeCount))
  }

  @_alwaysEmitIntoClient
  public var startIndex: Index { start }

  @_alwaysEmitIntoClient
  public var endIndex: Index { start.advanced(by: count) }

  @_alwaysEmitIntoClient
  func _checkBounds(_ position: Index) {
    precondition(startIndex <= position && position < endIndex, "Index out of bounds")
  }

  public subscript(position: Index) -> Element {
    @_alwaysEmitIntoClient
    get {
      _checkBounds(position)
      return self[unchecked: position]
    }
  }

  public subscript(unchecked position: Index) -> Element {
    @_alwaysEmitIntoClient
    get {
      return position._pointer.pointee
    }
  }

  @_alwaysEmitIntoClient
  public subscript(bounds: Range<Index>) -> BufferView<Element> {
    precondition(startIndex <= bounds.lowerBound && bounds.upperBound <= endIndex, "range out of bounds")
    return BufferView(start: bounds.lowerBound, count: bounds.lowerBound.distance(to: bounds.upperBound))
  }

  @_alwaysEmitIntoClient
  public func index(after i: Index) -> Index {
    return i.advanced(by: 1)
  }
}

