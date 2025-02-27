//===--- MutableSpan.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// A MutableSpan<Element> represents a span of memory which
// contains initialized `Element` instances.
@frozen
@available(SwiftStdlib 6.2, *)
public struct MutableSpan<Element: ~Copyable & ~Escapable>
: ~Copyable, ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  @usableFromInline
  internal let _count: Int

  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeMutableRawPointer {
    _pointer.unsafelyUnwrapped
  }

  @usableFromInline @inline(__always)
  @lifetime(borrow start)
  init(
    _unchecked start: UnsafeMutableRawPointer?,
    count: Int
  ) {
    _pointer = start
    _count = count
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan: @unchecked Sendable where Element: Sendable {}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: ~Copyable {

  @usableFromInline
  @lifetime(borrow elements)
  internal init(
    _unchecked elements: UnsafeMutableBufferPointer<Element>
  ) {
    _pointer = .init(elements.baseAddress)
    _count = elements.count
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeElements buffer: UnsafeMutableBufferPointer<Element>
  ) {
    precondition(
      ((Int(bitPattern: buffer.baseAddress) &
        (MemoryLayout<Element>.alignment&-1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    let ms = MutableSpan<Element>(_unchecked: buffer)
    self = _overrideLifetime(ms, borrowing: buffer)
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow start)
  public init(
    _unsafeStart start: UnsafeMutablePointer<Element>,
    count: Int
  ) {
    precondition(count >= 0, "Count must not be negative")
    let buffer = UnsafeMutableBufferPointer(start: start, count: count)
    let ms = MutableSpan(_unsafeElements: buffer)
    self = _overrideLifetime(ms, borrowing: start)
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan {

  @_alwaysEmitIntoClient
  @lifetime(borrow elements)
  public init(
    _unsafeElements elements: borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    let rb = UnsafeMutableBufferPointer(rebasing: elements)
    let ms = MutableSpan(_unsafeElements: rb)
    self = _overrideLifetime(ms, borrowing: elements)
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: BitwiseCopyable {

  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: UnsafeMutableRawBufferPointer
  ) {
    precondition(
      ((Int(bitPattern: buffer.baseAddress) &
        (MemoryLayout<Element>.alignment&-1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    let (byteCount, stride) = (buffer.count, MemoryLayout<Element>.stride)
    let (count, remainder) = byteCount.quotientAndRemainder(dividingBy: stride)
    precondition(remainder == 0, "Span must contain a whole number of elements")
    let elements = UnsafeMutableBufferPointer<Element>(
      start: buffer.baseAddress?.assumingMemoryBound(to: Element.self),
      count: count
    )
    let ms = MutableSpan(_unsafeElements: elements)
    self = _overrideLifetime(ms, borrowing: buffer)
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow pointer)
  public init(
    _unsafeStart pointer: UnsafeMutableRawPointer,
    byteCount: Int
  ) {
    precondition(byteCount >= 0, "Count must not be negative")
    let bytes = UnsafeMutableRawBufferPointer(start: pointer, count: byteCount)
    let ms = MutableSpan(_unsafeBytes: bytes)
    self = _overrideLifetime(ms, borrowing: pointer)
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    let bytes = UnsafeMutableRawBufferPointer(rebasing: buffer)
    let ms = MutableSpan(_unsafeBytes: bytes)
    self = _overrideLifetime(ms, borrowing: buffer)
  }
}

@available(SwiftStdlib 6.2, *)
extension Span where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @lifetime(borrow mutableSpan)
  public init(_unsafeMutableSpan mutableSpan: borrowing MutableSpan<Element>) {
    let pointer = mutableSpan._pointer?.assumingMemoryBound(to: Element.self)
    let buffer = UnsafeBufferPointer(start: pointer, count: mutableSpan.count)
    let span = Span(_unsafeElements: buffer)
    self = _overrideLifetime(span, borrowing: mutableSpan)
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var span: Span<Element> {
    @lifetime(borrow self)
    borrowing get {
      Span(_unsafeMutableSpan: self)
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension RawSpan {

  @_alwaysEmitIntoClient
  public init<Element: BitwiseCopyable>(
    _unsafeMutableSpan mutableSpan: borrowing MutableSpan<Element>
  ) {
    let pointer = mutableSpan._pointer
    let byteCount = mutableSpan.count &* MemoryLayout<Element>.stride
    let buffer = UnsafeRawBufferPointer(start: pointer, count: byteCount)
    let rawSpan = RawSpan(_unsafeBytes: buffer)
    self = _overrideLifetime(rawSpan, borrowing: mutableSpan)
  }
}

//@available(SwiftStdlib 6.2, *)
//extension MutableSpan where Element: Equatable {
//
//  @_alwaysEmitIntoClient
//  public func _elementsEqual(_ other: borrowing Self) -> Bool {
//    _elementsEqual(Span(_unsafeMutableSpan: other))
//  }
//
//  @_alwaysEmitIntoClient
//  public func _elementsEqual(_ other: Span<Element>) -> Bool {
//    Span(_unsafeMutableSpan: self)._elementsEqual(other)
//  }
//
//  @_alwaysEmitIntoClient
//  public func _elementsEqual(_ other: some Collection<Element>) -> Bool {
//    Span(_unsafeMutableSpan: self)._elementsEqual(other)
//  }
//
//  @_alwaysEmitIntoClient
//  public func _elementsEqual(_ other: some Sequence<Element>) -> Bool {
//    Span(_unsafeMutableSpan: self)._elementsEqual(other)
//  }
//}
//
@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var _description: String {
    let addr = String(UInt(bitPattern: _pointer), radix: 16, uppercase: false)
    return "(0x\(addr), \(_count))"
  }
}

//MARK: Collection, RandomAccessCollection
@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: ~Copyable & ~Escapable {

  @_alwaysEmitIntoClient
  public var count: Int { _count }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _count == 0 }

  public typealias Index = Int

  @_alwaysEmitIntoClient
  public var indices: Range<Index> {
    Range(uncheckedBounds: (0, _count))
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: BitwiseCopyable {

  /// Construct a RawSpan over the memory represented by this span
  ///
  /// - Returns: a RawSpan over the memory represented by this span
  @_alwaysEmitIntoClient
  public var bytes: RawSpan {
    @lifetime(borrow self)
    borrowing get {
      RawSpan(_unsafeMutableSpan: self)
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: ~Copyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(_ position: Index) -> Element {
    unsafeAddress {
      precondition(indices.contains(position), "index out of bounds")
      return UnsafePointer(_unsafeAddressOfElement(unchecked: position))
    }
    unsafeMutableAddress {
      precondition(indices.contains(position), "index out of bounds")
       return _unsafeAddressOfElement(unchecked: position)
    }
  }

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// This subscript does not validate `position`; this is an unsafe operation.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(unchecked position: Index) -> Element {
    unsafeAddress {
      UnsafePointer(_unsafeAddressOfElement(unchecked: position))
    }
    unsafeMutableAddress {
      _unsafeAddressOfElement(unchecked: position)
    }
  }

  @unsafe
  @_alwaysEmitIntoClient
  internal func _unsafeAddressOfElement(
    unchecked position: Index
  ) -> UnsafeMutablePointer<Element> {
    let elementOffset = position &* MemoryLayout<Element>.stride
    let address = _start().advanced(by: elementOffset)
    return address.assumingMemoryBound(to: Element.self)
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public mutating func swapAt(_ i: Index, _ j: Index) {
    precondition(indices.contains(Index(i)))
    precondition(indices.contains(Index(j)))
    swapAt(unchecked: i, unchecked: j)
  }

  @_alwaysEmitIntoClient
  public mutating func swapAt(unchecked i: Index, unchecked j: Index) {
    let pi = _unsafeAddressOfElement(unchecked: i)
    let pj = _unsafeAddressOfElement(unchecked: j)
    let temporary = pi.move()
    pi.initialize(to: pj.move())
    pj.initialize(to: consume temporary)
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: BitwiseCopyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(_ position: Index) -> Element {
    get {
      precondition(indices.contains(position), "index out of bounds")
      return self[unchecked: position]
    }
    set {
      precondition(indices.contains(position), "index out of bounds")
      self[unchecked: position] = newValue
    }
  }

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// This subscript does not validate `position`; this is an unsafe operation.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(unchecked position: Index) -> Element {
    get {
      let offset = position&*MemoryLayout<Element>.stride
      return _start().loadUnaligned(fromByteOffset: offset, as: Element.self)
    }
    set {
      let offset = position&*MemoryLayout<Element>.stride
      _start().storeBytes(of: newValue, toByteOffset: offset, as: Element.self)
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: ~Copyable {

  //FIXME: mark closure parameter as non-escaping
  @_alwaysEmitIntoClient
  public func withUnsafeBufferPointer<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    try Span(_unsafeMutableSpan: self).withUnsafeBufferPointer(body)
  }

  //FIXME: mark closure parameter as non-escaping
  @_alwaysEmitIntoClient
  public mutating func withUnsafeMutableBufferPointer<E: Error, Result: ~Copyable>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    guard let pointer = _pointer, count > 0 else {
      return try body(.init(start: nil, count: 0))
    }
    // bind memory by hand to sidestep alignment concerns
    let binding = Builtin.bindMemory(
      pointer._rawValue, count._builtinWordValue, Element.self
    )
    defer { Builtin.rebindMemory(pointer._rawValue, binding) }
    return try body(.init(start: .init(pointer._rawValue), count: count))
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: BitwiseCopyable {

  //FIXME: mark closure parameter as non-escaping
  @_alwaysEmitIntoClient
  public func withUnsafeBytes<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    try RawSpan(_unsafeMutableSpan: self).withUnsafeBytes(body)
  }

  //FIXME: mark closure parameter as non-escaping
  @_alwaysEmitIntoClient
  public mutating func withUnsafeMutableBytes<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeMutableRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    let bytes = UnsafeMutableRawBufferPointer(
      start: (_count == 0) ? nil : _start(),
      count: _count &* MemoryLayout<Element>.stride
    )
    return try body(bytes)
  }
}

//MARK: bulk-update functions
@available(SwiftStdlib 6.2, *)
extension MutableSpan {

  @_alwaysEmitIntoClient
  public mutating func update(repeating repeatedValue: consuming Element) {
    _start().withMemoryRebound(to: Element.self, capacity: count) {
      $0.update(repeating: repeatedValue, count: count)
    }
  }

  @_alwaysEmitIntoClient
  public mutating func update<S: Sequence>(
    from source: S
  ) -> (unwritten: S.Iterator, index: Index) where S.Element == Element {
    var iterator = source.makeIterator()
    let index = update(from: &iterator)
    return (iterator, index)
  }

  @_alwaysEmitIntoClient
  public mutating func update(
    from elements: inout some IteratorProtocol<Element>
  ) -> Index {
    var index = 0
    while index < _count {
      guard let element = elements.next() else { break }
      self[unchecked: index] = element
      index &+= 1
    }
    return index
  }

  @_alwaysEmitIntoClient
  public mutating func update(
    fromContentsOf source: some Collection<Element>
  ) -> Index {
    let updated = source.withContiguousStorageIfAvailable {
      self.update(fromContentsOf: Span(_unsafeElements: $0))
    }
    if let updated {
      return updated
    }

    //TODO: use _copyContents here

    var iterator = source.makeIterator()
    let index = update(from: &iterator)
    precondition(
      iterator.next() == nil,
      "destination buffer view cannot contain every element from source."
    )
    return index
  }

  @_alwaysEmitIntoClient
  public mutating func update(fromContentsOf source: Span<Element>) -> Index {
    guard !source.isEmpty else { return 0 }
    precondition(
      source.count <= self.count,
      "destination span cannot contain every element from source."
    )
    _start().withMemoryRebound(to: Element.self, capacity: source.count) { dest in
      source.withUnsafeBufferPointer {
        dest.update(from: $0.baseAddress!, count: $0.count)
      }
    }
    return source.count
  }

  @_alwaysEmitIntoClient
  public mutating func update(
    fromContentsOf source: borrowing MutableSpan<Element>
  ) -> Index {
    update(fromContentsOf: source.span)
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: ~Copyable {

//  @_alwaysEmitIntoClient
//  public mutating func moveUpdate(
//    fromContentsOf source: consuming OutputSpan<Element>
//  ) -> Index {
//    guard !source.isEmpty else { return 0 }
//    precondition(
//      source.count <= self.count,
//      "destination span cannot contain every element from source."
//    )
//    let buffer = source.relinquishBorrowedMemory()
//    // we must now deinitialize the returned UMBP
//    _start().moveInitializeMemory(
//      as: Element.self, from: buffer.baseAddress!, count: buffer.count
//    )
//    return buffer.count
//  }

  @_alwaysEmitIntoClient
  public mutating func moveUpdate(
    fromContentsOf source: UnsafeMutableBufferPointer<Element>
  ) -> Index {
//    let source = OutputSpan(_initializing: source, initialized: source.count)
//    return self.moveUpdate(fromContentsOf: source)
    withUnsafeMutableBufferPointer {
      $0.moveUpdate(fromContentsOf: source)
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan {

  @_alwaysEmitIntoClient
  public mutating func moveUpdate(
    fromContentsOf source: Slice<UnsafeMutableBufferPointer<Element>>
  ) -> Index {
    moveUpdate(fromContentsOf: UnsafeMutableBufferPointer(rebasing: source))
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableSpan where Element: BitwiseCopyable {

  @_alwaysEmitIntoClient
  public mutating func update(
    repeating repeatedValue: Element
  ) where Element: BitwiseCopyable {
    guard count > 0 else { return }
    // rebind _start manually in order to avoid assumptions about alignment.
    let rp = _start()._rawValue
    let binding = Builtin.bindMemory(rp, count._builtinWordValue, Element.self)
    UnsafeMutablePointer(rp).update(repeating: repeatedValue, count: count)
    Builtin.rebindMemory(rp, binding)
  }

  @_alwaysEmitIntoClient
  public mutating func update<S: Sequence>(
    from source: S
  ) -> (unwritten: S.Iterator, index: Index)
  where S.Element == Element, Element: BitwiseCopyable {
    var iterator = source.makeIterator()
    let index = update(from: &iterator)
    return (iterator, index)
  }

  @_alwaysEmitIntoClient
  public mutating func update(
    from elements: inout some IteratorProtocol<Element>
  ) -> Index {
    var index = 0
    while index < _count {
      guard let element = elements.next() else { break }
      self[unchecked: index] = element
      index &+= 1
    }
    return index
  }

  @_alwaysEmitIntoClient
  public mutating func update(
    fromContentsOf source: some Collection<Element>
  ) -> Index where Element: BitwiseCopyable {
    let updated = source.withContiguousStorageIfAvailable {
      self.update(fromContentsOf: Span(_unsafeElements: $0))
    }
    if let updated {
      return updated
    }

    //TODO: use _copyContents here

    var iterator = source.makeIterator()
    let index = update(from: &iterator)
    precondition(
      iterator.next() == nil,
      "destination buffer view cannot contain every element from source."
    )
    return index
  }

  @_alwaysEmitIntoClient
  public mutating func update(
    fromContentsOf source: Span<Element>
  ) -> Index where Element: BitwiseCopyable {
    guard !source.isEmpty else { return 0 }
    precondition(
      source.count <= self.count,
      "destination span cannot contain every element from source."
    )
    source.withUnsafeBufferPointer {
      _start().copyMemory(
        from: $0.baseAddress!, byteCount: $0.count&*MemoryLayout<Element>.stride
      )
    }
    return source.count
  }

  @_alwaysEmitIntoClient
  public mutating func update(
    fromContentsOf source: borrowing MutableSpan<Element>
  ) -> Index where Element: BitwiseCopyable {
    update(fromContentsOf: source.span)
  }
}
