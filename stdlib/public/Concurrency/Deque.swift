//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.5, *)
@frozen
@usableFromInline
struct _Deque<Element> {
  @frozen
  @usableFromInline
  internal struct _UnsafeHandle {
    @usableFromInline
    let _header: UnsafeMutablePointer<_Storage._Header>
    @usableFromInline
    let _elements: UnsafeMutablePointer<Element>?

    @inlinable
    @inline(__always)
    init(
      header: UnsafeMutablePointer<_Storage._Header>,
      elements: UnsafeMutablePointer<Element>?,
      isMutable: Bool
    ) {
      self._header = header
      self._elements = elements
    }
    
    @inlinable
    @inline(__always)
    var header: _Storage._Header {
      _header.pointee
    }
    
    @inlinable
    @inline(__always)
    var capacity: Int {
      _header.pointee.capacity
    }
    
    @inlinable
    @inline(__always)
    var count: Int {
      get { _header.pointee.count }
      nonmutating set { _header.pointee.count = newValue }
    }
    
    @inlinable
    internal func slot(after slot: Int) -> Int {
      assert(slot < capacity)
      let position = slot + 1
      if position >= capacity {
        return 0
      }
      return position
    }

    
    @inlinable
    internal func slot(_ slot: Int, offsetBy delta: Int) -> Int {
      assert(slot <= capacity)
      let position = slot + delta
      if delta >= 0 {
        if position >= capacity { return position - capacity }
      } else {
        if position < 0 { return position + capacity }
      }
      return position
    }
    
    @inlinable
    @inline(__always)
    internal var endSlot: Int {
      slot(startSlot, offsetBy: count)
    }
    
    @inlinable
    internal func uncheckedAppend(_ element: Element) {
      assert(count < capacity)
      ptr(at: endSlot).initialize(to: element)
      count += 1
    }
    
    @inlinable
    internal func uncheckedRemoveFirst() -> Element {
      assert(count > 0)
      let result = ptr(at: startSlot).move()
      startSlot = slot(after: startSlot)
      count -= 1
      return result
    }
    
    @inlinable
    internal func uncheckedRemoveFirstIfPresent() -> Element? {
      if count > 0 {
        let result = ptr(at: startSlot).move()
        startSlot = slot(after: startSlot)
        count -= 1
        return result
      } else {
        return nil
      }
    }
    
    @frozen
    @usableFromInline
    struct _UnsafeWrappedBuffer {
      @usableFromInline
      internal let first: UnsafeBufferPointer<Element>

      @usableFromInline
      internal let second: UnsafeBufferPointer<Element>?

      @inlinable
      @inline(__always)
      internal init(
        _ first: UnsafeBufferPointer<Element>,
        _ second: UnsafeBufferPointer<Element>? = nil
      ) {
        self.first = first
        self.second = second
        assert(first.count > 0 || second == nil)
      }

      @inlinable
      internal init(
        start: UnsafePointer<Element>,
        count: Int
      ) {
        self.init(UnsafeBufferPointer(start: start, count: count))
      }
      
      @inlinable
      internal init(
        first start1: UnsafePointer<Element>,
        count count1: Int,
        second start2: UnsafePointer<Element>,
        count count2: Int
      ) {
        self.init(UnsafeBufferPointer(start: start1, count: count1),
                  UnsafeBufferPointer(start: start2, count: count2))
      }

      @inlinable
      internal var count: Int { first.count + (second?.count ?? 0) }
    }
    
    @frozen
    @usableFromInline
    internal struct _UnsafeMutableWrappedBuffer {
      @usableFromInline
      internal let first: UnsafeMutableBufferPointer<Element>

      @usableFromInline
      internal let second: UnsafeMutableBufferPointer<Element>?

      @inlinable
      @inline(__always)
      internal init(
        _ first: UnsafeMutableBufferPointer<Element>,
        _ second: UnsafeMutableBufferPointer<Element>? = nil
      ) {
        self.first = first
        self.second = second?.count == 0 ? nil : second
        assert(first.count > 0 || second == nil)
      }

      @inlinable
      @inline(__always)
      internal init(
        start: UnsafeMutablePointer<Element>,
        count: Int
      ) {
        self.init(UnsafeMutableBufferPointer(start: start, count: count))
      }

      @inlinable
      @inline(__always)
      internal init(
        first start1: UnsafeMutablePointer<Element>,
        count count1: Int,
        second start2: UnsafeMutablePointer<Element>,
        count count2: Int
      ) {
        self.init(UnsafeMutableBufferPointer(start: start1, count: count1),
                  UnsafeMutableBufferPointer(start: start2, count: count2))
      }

      @inlinable
      @inline(__always)
      internal init(mutating buffer: _UnsafeWrappedBuffer) {
        self.init(.init(mutating: buffer.first),
                  buffer.second.map { .init(mutating: $0) })
      }
    }
    
    @inlinable
    internal func segments() -> _UnsafeWrappedBuffer {
      let wrap = capacity - startSlot
      if count <= wrap {
        return .init(start: ptr(at: startSlot), count: count)
      }
      return .init(first: ptr(at: startSlot), count: wrap,
                   second: ptr(at: .zero), count: count - wrap)
    }
    
    @inlinable
    @inline(__always)
    internal func mutableSegments() -> _UnsafeMutableWrappedBuffer {
      return .init(mutating: segments())
    }
    
    @inlinable
    @inline(__always)
    var startSlot: Int {
      get { _header.pointee.startSlot }
      nonmutating set { _header.pointee.startSlot = newValue }
    }
    
    @inlinable
    @inline(__always)
    func ptr(at slot: Int) -> UnsafeMutablePointer<Element> {
      assert(slot >= 0 && slot <= capacity)
      return _elements! + slot
    }
    
    
    
    @inlinable
    @discardableResult
    func initialize(
      at start: Int,
      from source: UnsafeBufferPointer<Element>
    ) -> Int {
      assert(start + source.count <= capacity)
      guard source.count > 0 else { return start }
      ptr(at: start).initialize(from: source.baseAddress!, count: source.count)
      return start + source.count
    }
    
    @inlinable
    @inline(__always)
    @discardableResult
    func moveInitialize(
      at start: Int,
      from source: UnsafeMutableBufferPointer<Element>
    ) -> Int {
      assert(start + source.count <= capacity)
      guard source.count > 0 else { return start }
      ptr(at: start).moveInitialize(from: source.baseAddress!, count: source.count)
      return start + source.count
    }
    
    @inlinable
    internal func copyElements() -> _Storage {
      let object = _Storage._DequeBuffer.create(
        minimumCapacity: capacity,
        makingHeaderWith: { _ in header })
      let result = _Storage(_buffer: ManagedBufferPointer(unsafeBufferObject: object))
      guard self.count > 0 else { return result }
      result.update { target in
        let source = self.segments()
        target.initialize(at: startSlot, from: source.first)
        if let second = source.second {
          target.initialize(at: 0, from: second)
        }
      }
      return result
    }
    
    @inlinable
    internal func moveElements(minimumCapacity: Int) -> _Storage {
      let count = self.count
      assert(minimumCapacity >= count)
      let object = _Storage._DequeBuffer.create(
        minimumCapacity: minimumCapacity,
        makingHeaderWith: {
          _Storage._Header(
            capacity: $0.capacity,
            count: count,
            startSlot: .zero)
        })
      let result = _Storage(_buffer: ManagedBufferPointer(unsafeBufferObject: object))
      guard count > 0 else { return result }
      result.update { target in
        let source = self.mutableSegments()
        let next = target.moveInitialize(at: .zero, from: source.first)
        if let second = source.second {
          target.moveInitialize(at: next, from: second)
        }
      }
      self.count = 0
      return result
    }
  }
  
  @frozen
  @usableFromInline
  enum _Storage {
    @usableFromInline
    internal struct _Header {
      @usableFromInline
      var capacity: Int

      @usableFromInline
      var count: Int

      @usableFromInline
      var startSlot: Int

      @usableFromInline
      init(capacity: Int, count: Int, startSlot: Int) {
        self.capacity = capacity
        self.count = count
        self.startSlot = startSlot
      }
    }
    
    @usableFromInline
    internal typealias _Buffer = ManagedBufferPointer<_Header, Element>

    case empty
    case buffer(_Buffer)

    @_fixed_layout
    @usableFromInline
    internal class _DequeBuffer: ManagedBuffer<_Header, Element> {
      @inlinable
      deinit {
        self.withUnsafeMutablePointers { header, elements in
          let capacity = header.pointee.capacity
          let count = header.pointee.count
          let startSlot = header.pointee.startSlot

          if startSlot + count <= capacity {
            (elements + startSlot).deinitialize(count: count)
          } else {
            let firstRegion = capacity - startSlot
            (elements + startSlot).deinitialize(count: firstRegion)
            elements.deinitialize(count: count - firstRegion)
          }
        }
      }
    }
    
    @inlinable
    @inline(__always)
    internal init(_buffer: _Buffer) {
      self = .buffer(_buffer)
    }
    
    @inlinable
    internal init() {
      self = .empty
    }
    
    @inlinable
    internal init(_ object: _DequeBuffer) {
      self.init(_buffer: _Buffer(unsafeBufferObject: object))
    }
    
    @inlinable
    @inline(__always)
    internal var capacity: Int {
      switch self {
      case .empty: return 0
      case .buffer(let buffer):
        return buffer.withUnsafeMutablePointerToHeader { $0.pointee.capacity }
      }
      
    }
    
    @inlinable
    @inline(__always)
    internal mutating func ensure(
      minimumCapacity: Int
    ) {
      if _slowPath(capacity < minimumCapacity) {
        _ensure(minimumCapacity: minimumCapacity)
      }
    }
    
    @inlinable
    @inline(__always)
    internal static var growthFactor: Double { 1.5 }

    @usableFromInline
    internal func _growCapacity(
      to minimumCapacity: Int
    ) -> Int {
      return Swift.max(Int((Self.growthFactor * Double(capacity)).rounded(.up)),
                       minimumCapacity)
    }
    
    @inlinable
    internal mutating func _ensure(
      minimumCapacity: Int
    ) {
      if capacity >= minimumCapacity {
        self = self.read { $0.copyElements() }
      } else {
        let minimumCapacity = _growCapacity(to: minimumCapacity)
        self = self.update { source in
          source.moveElements(minimumCapacity: minimumCapacity)
        }
      }
    }
    
    @inlinable
    @inline(__always)
    internal var count: Int {
      switch self {
      case .empty: return 0
      case .buffer(let buffer):
        return buffer.withUnsafeMutablePointerToHeader { $0.pointee.count }
      }
      
    }
    
    @inlinable
    @inline(__always)
    internal func read<R>(_ body: (_UnsafeHandle) throws -> R) rethrows -> R {
      switch self {
      case .empty:
        var header = _Header(capacity: 0, count: 0, startSlot: 0)
        return try withUnsafeMutablePointer(to: &header) { headerPtr in
          return try body(_UnsafeHandle(header: headerPtr, elements: nil, isMutable: false))
        }
      case .buffer(let buffer):
        return try buffer.withUnsafeMutablePointers { header, elements in
          let handle = _UnsafeHandle(header: header,
                                     elements: elements,
                                     isMutable: false)
          return try body(handle)
        }
      }
      
    }
    
    @inlinable
    @inline(__always)
    internal func update<R>(_ body: (_UnsafeHandle) throws -> R) rethrows -> R {
      switch self {
      case .empty:
        var header = _Header(capacity: 0, count: 0, startSlot: 0)
        return try withUnsafeMutablePointer(to: &header) { headerPtr in
          return try body(_UnsafeHandle(header: headerPtr, elements: nil, isMutable: false))
        }
      case .buffer(let buffer):
        return try buffer.withUnsafeMutablePointers { header, elements in
          let handle = _UnsafeHandle(header: header,
                                     elements: elements,
                                     isMutable: true)
          return try body(handle)
        }
      }
    }
  }
  

  @usableFromInline
  internal var _storage: _Storage
  
  @inlinable
  init() {
    _storage = _Storage()
  }
  
  @inlinable
  @inline(__always)
  var count: Int { _storage.count }
  
  @inlinable
  mutating func append(_ newElement: Element) {
    _storage.ensure(minimumCapacity: _storage.count + 1)
    _storage.update {
      $0.uncheckedAppend(newElement)
    }
  }
  
  @inlinable
  @discardableResult
  mutating func removeFirst() -> Element {
    return _storage.update { $0.uncheckedRemoveFirst() }
  }
  
  @inlinable
  @discardableResult
  mutating func removeFirstIfPresent() -> Element? {
    return _storage.update { $0.uncheckedRemoveFirstIfPresent() }
  }
}

