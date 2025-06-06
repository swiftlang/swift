import Builtin

// A MutableSpan<Element> represents a span of memory which
// contains initialized `Element` instances.
@frozen
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
public struct MutableSpan<Element: ~Copyable>: ~Copyable & ~Escapable {
  @usableFromInline let _pointer: UnsafeMutableRawPointer?

  @usableFromInline let _count: Int

  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeMutableRawPointer {
    _pointer.unsafelyUnwrapped
  }

  @usableFromInline @inline(__always)
  @_lifetime(borrow start)
  init(
    _unchecked start: UnsafeMutableRawPointer?,
    count: Int
  ) {
    _pointer = start
    _count = count
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan: @unchecked Sendable where Element: Sendable {}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @usableFromInline @inline(__always)
  @_lifetime(borrow elements)
  internal init(
    _unchecked elements: UnsafeMutableBufferPointer<Element>
  ) {
    _pointer = .init(elements.baseAddress)
    _count = elements.count
  }

  @_alwaysEmitIntoClient
  @_lifetime(borrow buffer)
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
  @_lifetime(borrow start)
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

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan {

  @_alwaysEmitIntoClient
  @_lifetime(borrow elements)
  public init(
    _unsafeElements elements: borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    let rb = UnsafeMutableBufferPointer(rebasing: elements)
    let ms = MutableSpan(_unsafeElements: rb)
    self = _overrideLifetime(ms, borrowing: elements)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan where Element: BitwiseCopyable {

  @_alwaysEmitIntoClient
  @_lifetime(borrow buffer)
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
  @_lifetime(borrow pointer)
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
  @_lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    let bytes = UnsafeMutableRawBufferPointer(rebasing: buffer)
    let ms = MutableSpan(_unsafeBytes: bytes)
    self = _overrideLifetime(ms, borrowing: buffer)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension Span where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @_lifetime(borrow mutableSpan)
  public init(_unsafeMutableSpan mutableSpan: borrowing MutableSpan<Element>) {
    let pointer = mutableSpan._pointer?.assumingMemoryBound(to: Element.self)
    let buffer = UnsafeBufferPointer(start: pointer, count: mutableSpan.count)
    let span = Span(_unsafeElements: buffer)
    self = _overrideLifetime(span, borrowing: mutableSpan)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var storage: Span<Element> {
    @_lifetime(borrow self)
    borrowing get {
      Span(_unsafeMutableSpan: self)
    }
  }

  @_alwaysEmitIntoClient
  public func withSpan<E: Error, Result: ~Copyable>(
    _ body: (Span<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    try body(Span(_unsafeMutableSpan: self))
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension RawSpan {

  @_alwaysEmitIntoClient
  @_lifetime(borrow mutableSpan)
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

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan where Element: Equatable {

  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: borrowing Self) -> Bool {
    _elementsEqual(Span(_unsafeMutableSpan: other))
  }

  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: Span<Element>) -> Bool {
    Span(_unsafeMutableSpan: self)._elementsEqual(other)
  }

  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: some Collection<Element>) -> Bool {
    Span(_unsafeMutableSpan: self)._elementsEqual(other)
  }

  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: some Sequence<Element>) -> Bool {
    Span(_unsafeMutableSpan: self)._elementsEqual(other)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var _description: String {
    let addr = String(UInt(bitPattern: _pointer), radix: 16, uppercase: false)
    return "(0x\(addr), \(_count))"
  }
}

//MARK: Collection, RandomAccessCollection
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan where Element: ~Copyable {

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

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan where Element: BitwiseCopyable {

  /// Construct a RawSpan over the memory represented by this span
  ///
  /// - Returns: a RawSpan over the memory represented by this span
  @unsafe //FIXME: remove when the lifetime inference is fixed
  @_alwaysEmitIntoClient
  public var _unsafeRawSpan: RawSpan {
    @_lifetime(borrow self)
    get { RawSpan(_unsafeMutableSpan: self) }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan where Element: ~Copyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(_ position: Index) -> Element {
    @_lifetime(borrow self)
    _read {
      precondition(indices.contains(position), "index out of bounds")
      yield self[unchecked: position]
    }
    @_lifetime(&self)
    _modify {
      precondition(indices.contains(position), "index out of bounds")
      yield &self[unchecked: position]
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
    @_lifetime(self: copy self)
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

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan where Element: ~Copyable {

  @_lifetime(self: copy self)
  public mutating func swapAt(_ i: Index, _ j: Index) {
    precondition(indices.contains(Index(i)))
    precondition(indices.contains(Index(j)))
    swapAt(unchecked: i, unchecked: j)
  }

  @_lifetime(self: copy self)
  public mutating func swapAt(unchecked i: Index, unchecked j: Index) {
    let pi = _unsafeAddressOfElement(unchecked: i)
    let pj = _unsafeAddressOfElement(unchecked: j)
    let temporary = pi.move()
    pi.initialize(to: pj.move())
    pj.initialize(to: consume temporary)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
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
      precondition(indices.contains(position))
      return self[unchecked: position]
    }
    @_lifetime(self: copy self)
    set {
      precondition(indices.contains(position))
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
    @_lifetime(self: copy self)
    set {
      let offset = position&*MemoryLayout<Element>.stride
      _start().storeBytes(of: newValue, toByteOffset: offset, as: Element.self)
    }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
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
  @_lifetime(self: copy self)
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

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
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
  @_lifetime(self: copy self)
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
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension MutableSpan {

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func update(repeating repeatedValue: Element) {
    _start().withMemoryRebound(to: Element.self, capacity: count) {
      $0.update(repeating: repeatedValue, count: count)
    }
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func update<S: Sequence>(
    from source: S
  ) -> (unwritten: S.Iterator, index: Index) where S.Element == Element {
    var iterator = source.makeIterator()
    let index = update(from: &iterator)
    return (iterator, index)
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
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
  @_lifetime(self: copy self)
  public mutating func update(
    fromContentsOf source: some Collection<Element>
  ) -> Index {
    let updated = source.withContiguousStorageIfAvailable {
      self.update(fromContentsOf: Span(_unsafeElements: $0))
    }
    if let updated {
      return updated
    }

    var iterator = source.makeIterator()
    let index = update(from: &iterator)
    precondition(
      iterator.next() == nil,
      "destination buffer view cannot contain every element from source."
    )
    return index
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
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
  @_lifetime(self: copy self)
  public mutating func update(
    fromContentsOf source: borrowing MutableSpan<Element>
  ) -> Index {
    update(fromContentsOf: source.storage)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)

extension MutableSpan where Element: BitwiseCopyable {

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
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
  @_lifetime(self: copy self)
  public mutating func update<S: Sequence>(
    from source: S
  ) -> (unwritten: S.Iterator, index: Index)
  where S.Element == Element, Element: BitwiseCopyable {
    var iterator = source.makeIterator()
    let index = update(from: &iterator)
    return (iterator, index)
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
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
  @_lifetime(self: copy self)
  public mutating func update(
    fromContentsOf source: some Collection<Element>
  ) -> Index where Element: BitwiseCopyable {
    let updated = source.withContiguousStorageIfAvailable {
      self.update(fromContentsOf: Span(_unsafeElements: $0))
    }
    if let updated {
      return updated
    }

    var iterator = source.makeIterator()
    let index = update(from: &iterator)
    precondition(
      iterator.next() == nil,
      "destination buffer view cannot contain every element from source."
    )
    return index
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
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
  @_lifetime(self: copy self)
  public mutating func update(
    fromContentsOf source: borrowing MutableSpan<Element>
  ) -> Index where Element: BitwiseCopyable {
    update(fromContentsOf: source.storage)
  }
}

@frozen
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
public struct OutputSpan<Element: ~Copyable>: ~Copyable, ~Escapable {
  @usableFromInline let _pointer: UnsafeMutableRawPointer?

  public let capacity: Int

  @usableFromInline
  var _initialized: Int = 0

  @_alwaysEmitIntoClient
  @usableFromInline @inline(__always)
  var _start: UnsafeMutableRawPointer { _pointer.unsafelyUnwrapped }

  @_alwaysEmitIntoClient
  public var available: Int { capacity &- _initialized }

  @_alwaysEmitIntoClient
  public var count: Int { _initialized }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _initialized == 0 }

  deinit {
    if _initialized > 0 {
      _start.withMemoryRebound(to: Element.self, capacity: _initialized) {
        [ workaround = _initialized ] in
        _ = $0.deinitialize(count: workaround)
      }
    }
  }

  @usableFromInline @inline(__always)
  @_lifetime(borrow start)
  init(
    _unchecked start: UnsafeMutableRawPointer?,
    capacity: Int,
    initialized: Int
  ) {
    _pointer = start
    self.capacity = capacity
    _initialized = initialized
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
@available(*, unavailable)
extension OutputSpan: Sendable {}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension OutputSpan where Element: ~Copyable {

@available(macOS 9999, *)
@available(macOS 9999, *)
@available(macOS 9999, *)
  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append(_ value: consuming Element) {
    precondition(_initialized < capacity, "Output buffer overflow")
    let p = _start.advanced(by: _initialized&*MemoryLayout<Element>.stride)
    p.initializeMemory(as: Element.self, to: value)
    _initialized &+= 1
  }

  @_alwaysEmitIntoClient
  public mutating func deinitializeLastElement() -> Element? {
    guard _initialized > 0 else { return nil }
    _initialized &-= 1
    let p = _start.advanced(by: _initialized&*MemoryLayout<Element>.stride)
    return p.withMemoryRebound(to: Element.self, capacity: 1, { $0.move() })
  }

  @_alwaysEmitIntoClient
  public mutating func deinitialize() {
    _ = _start.withMemoryRebound(to: Element.self, capacity: _initialized) {
      $0.deinitialize(count: _initialized)
    }
    _initialized = 0
  }
}

//MARK: bulk-update functions
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension OutputSpan {

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append(repeating repeatedValue: Element, count: Int) {
    let available = capacity &- _initialized
    precondition(
      count <= available,
      "destination span cannot contain number of elements requested."
    )
    let offset = _initialized&*MemoryLayout<Element>.stride
    let p = _start.advanced(by: offset)
    p.withMemoryRebound(to: Element.self, capacity: count) {
      $0.initialize(repeating: repeatedValue, count: count)
    }
    _initialized &+= count
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append<S>(
    from elements: S
  ) -> S.Iterator where S: Sequence, S.Element == Element {
    var iterator = elements.makeIterator()
    append(from: &iterator)
    return iterator
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append(
    from elements: inout some IteratorProtocol<Element>
  ) {
    while _initialized < capacity {
      guard let element = elements.next() else { break }
      let p = _start.advanced(by: _initialized&*MemoryLayout<Element>.stride)
      p.initializeMemory(as: Element.self, to: element)
      _initialized &+= 1
    }
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: some Collection<Element>
  ) {
    let void: Void? = source.withContiguousStorageIfAvailable {
#if false
      append(fromContentsOf: Span(_unsafeElements: $0))
#else //FIXME: remove once rdar://136838539 & rdar://136849171 are fixed
      append(fromContentsOf: $0)
#endif
    }
    if void != nil {
      return
    }

    let available = capacity &- _initialized
    let tail = _start.advanced(by: _initialized&*MemoryLayout<Element>.stride)
    var (iterator, copied) =
    tail.withMemoryRebound(to: Element.self, capacity: available) {
      let suffix = UnsafeMutableBufferPointer(start: $0, count: available)
      return source._copyContents(initializing: suffix)
    }
    precondition(
      iterator.next() == nil,
      "destination span cannot contain every element from source."
    )
    assert(_initialized + copied <= capacity) // invariant check
    _initialized &+= copied
  }

  //FIXME: remove once rdar://136838539 & rdar://136849171 are fixed
  @_lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: UnsafeBufferPointer<Element>
  ) {
    guard !source.isEmpty else { return }
    precondition(
      source.count <= available,
      "destination span cannot contain every element from source."
    )
    let tail = _start.advanced(by: _initialized&*MemoryLayout<Element>.stride)
    source.baseAddress!.withMemoryRebound(to: Element.self, capacity: source.count) {
      _ = tail.initializeMemory(as: Element.self, from: $0, count: source.count)
    }
    _initialized += source.count
  }

  //FIXME: rdar://136838539 & rdar://136849171
  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: Span<Element>
  ) {
    guard !source.isEmpty else { return }
    precondition(
      source.count <= available,
      "destination span cannot contain every element from source."
    )
    let tail = _start.advanced(by: _initialized&*MemoryLayout<Element>.stride)
    _ = source.withUnsafeBufferPointer {
      tail.initializeMemory(
        as: Element.self, from: $0.baseAddress!, count: $0.count
      )
    }
    _initialized += source.count
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append(fromContentsOf source: borrowing MutableSpan<Element>) {
    source.withUnsafeBufferPointer { append(fromContentsOf: $0) }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func moveAppend(
    fromContentsOf source: consuming Self
  ) {
    guard !source.isEmpty else { return }
    precondition(
      source.count <= available,
      "buffer cannot contain every element from source."
    )
    let buffer = source.relinquishBorrowedMemory()
    // we must now deinitialize the returned UMBP
    let tail = _start.advanced(by: _initialized&*MemoryLayout<Element>.stride)
    tail.moveInitializeMemory(
      as: Element.self, from: buffer.baseAddress!, count: buffer.count
    )
    _initialized &+= buffer.count
  }

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func moveAppend(
    fromContentsOf source: UnsafeMutableBufferPointer<Element>
  ) {
#if false //FIXME: rdar://136838539 & rdar://136849171
    let source = OutputSpan(_initializing: source, initialized: source.count)
    moveAppend(fromContentsOf: source)
#else
    guard !source.isEmpty else { return }
    precondition(
      source.count <= available,
      "buffer cannot contain every element from source."
    )
    let tail = _start.advanced(by: _initialized&*MemoryLayout<Element>.stride)
    tail.moveInitializeMemory(
      as: Element.self, from: source.baseAddress!, count: source.count
    )
    _initialized &+= source.count
#endif
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension OutputSpan {

  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func moveAppend(
    fromContentsOf source: Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    moveAppend(fromContentsOf: UnsafeMutableBufferPointer(rebasing: source))
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension OutputSpan where Element: BitwiseCopyable {

}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var span: Span<Element> {
    @_lifetime(borrow self)
    borrowing get {
      let pointer = _pointer?.assumingMemoryBound(to: Element.self)
      let buffer = UnsafeBufferPointer(start: pointer, count: _initialized)
      let span = Span(_unsafeElements: buffer)
      return _overrideLifetime(span, borrowing: self)
    }
  }

  /* FIXME: rdar://147194789 ([nonescapable] 'mutating get' causes a
     type checking error for non-existent _read accessor)
  @_alwaysEmitIntoClient
  public var mutableSpan: MutableSpan<Element> {
    @_lifetime(&self)
    mutating get { // the accessor must provide a mutable projection
      let pointer = _pointer?.assumingMemoryBound(to: Element.self)
      let buffer = UnsafeMutableBufferPointer(start: pointer, count: _initialized)
      let span = MutableSpan(_unsafeElements: buffer)
      return _overrideLifetime(span, mutating: &self)
    }
  }
   */
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public consuming func relinquishBorrowedMemory() -> UnsafeMutableBufferPointer<Element> {
    let (start, count) = (self._pointer, self._initialized)
    discard self
    let typed = start?.bindMemory(to: Element.self, capacity: count)
    return .init(start: typed, count: count)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension OutputSpan where Element: BitwiseCopyable {

  @_alwaysEmitIntoClient
  public consuming func relinquishBorrowedBytes() -> UnsafeMutableRawBufferPointer {
    let (start, count) = (self._pointer, self._initialized)
    discard self
    return .init(start: start, count: count&*MemoryLayout<Element>.stride)
  }
}

private let immortalThing = ""

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension Span {

//  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
//  public static var empty: Span {
//    @_lifetime(immortal)
//    get {
//      let nilBasedBuffer = UnsafeBufferPointer<Element>(start: nil, count: 0)
//      let span = Span(_unsafeElements: nilBasedBuffer)
//      return _overrideLifetime(span, to: immortalThing)
//    }
//  }
//
//  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
//  @_lifetime(immortal)
//  public init() {
//    let nilBasedBuffer = UnsafeBufferPointer<Element>(start: nil, count: 0)
//    let span = Span(_unsafeElements: nilBasedBuffer)
//    self = _overrideLifetime(span, to: immortalThing)
//  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
extension Span where Element: Equatable {

  /// Returns a Boolean value indicating whether this and another span
  /// contain equal elements in the same order.
  ///
  /// - Parameters:
  ///   - other: A span to compare to this one.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: Self) -> Bool {
    guard count == other.count else { return false }
    if count == 0 { return true }

    //FIXME: This could be short-cut
    //       with a layout constraint where stride equals size,
    //       as long as there is at most 1 unused bit pattern.
    // if Element is BitwiseEquatable {
    // return _swift_stdlib_memcmp(lhs.baseAddress, rhs.baseAddress, count) == 0
    // }
    for o in 0..<count {
      if self[unchecked: o] != other[unchecked: o] { return false }
    }
    return true
  }

  /// Returns a Boolean value indicating whether this span and a Collection
  /// contain equal elements in the same order.
  ///
  /// - Parameters:
  ///   - other: A Collection to compare to this span.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: some Collection<Element>) -> Bool {
    let equal = other.withContiguousStorageIfAvailable {
      _elementsEqual(Span(_unsafeElements: $0))
    }
    if let equal { return equal }

    guard count == other.count else { return false }
    if count == 0 { return true }

    return _elementsEqual(AnySequence(other))
  }

  /// Returns a Boolean value indicating whether this span and a Sequence
  /// contain equal elements in the same order.
  ///
  /// - Parameters:
  ///   - other: A Sequence to compare to this span.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: some Sequence<Element>) -> Bool {
    var offset = 0
    for otherElement in other {
      if offset >= count { return false }
      if self[unchecked: offset] != otherElement { return false }
      offset += 1
    }
    return offset == count
  }
}

