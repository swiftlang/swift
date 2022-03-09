//===----------------------------------------------------------*- swift -*-===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

extension Slice where Base == UnsafeMutableRawBufferPointer {

  /// Copies the bytes from the given buffer to this buffer slice's memory.
  ///
  /// If the `source.count` bytes of memory referenced by this buffer are bound
  /// to a type `T`, then `T` must be a trivial type, the underlying pointer
  /// must be properly aligned for accessing `T`, and `source.count` must be a
  /// multiple of `MemoryLayout<T>.stride`.
  ///
  /// The memory referenced by `source` may overlap with the memory referenced
  /// by this buffer.
  ///
  /// After calling `copyMemory(from:)`, the first `source.count` bytes of
  /// memory referenced by this buffer are initialized to raw bytes. If the
  /// memory is bound to type `T`, then it contains values of type `T`.
  ///
  /// - Parameter source: A buffer of raw bytes. `source.count` must
  ///   be less than or equal to this buffer slice's `count`.
  @inlinable
  @_alwaysEmitIntoClient
  func copyMemory(from source: UnsafeRawBufferPointer) {
    let buffer = Base(rebasing: self)
    buffer.copyMemory(from: source)
  }

  /// Copies from a collection of `UInt8` into this buffer slice's memory.
  ///
  /// If the `source.count` bytes of memory referenced by this buffer are bound
  /// to a type `T`, then `T` must be a trivial type, the underlying pointer
  /// must be properly aligned for accessing `T`, and `source.count` must be a
  /// multiple of `MemoryLayout<T>.stride`.
  ///
  /// After calling `copyBytes(from:)`, the first `source.count` bytes of memory
  /// referenced by this buffer are initialized to raw bytes. If the memory is
  /// bound to type `T`, then it contains values of type `T`.
  ///
  /// - Parameter source: A collection of `UInt8` elements. `source.count` must
  ///   be less than or equal to this buffer slice's `count`.
  @inlinable
  @_alwaysEmitIntoClient
  public func copyBytes<C: Collection>(
    from source: C
  ) where C.Element == UInt8 {
    let buffer = Base(rebasing: self)
    buffer.copyBytes(from: source)
  }

  /// Initializes the memory referenced by this buffer with the given value,
  /// binds the memory to the value's type, and returns a typed buffer of the
  /// initialized memory.
  ///
  /// The memory referenced by this buffer must be uninitialized or
  /// initialized to a trivial type, and must be properly aligned for
  /// accessing `T`.
  ///
  /// After calling this method on a raw buffer with non-nil `baseAddress` `b`,
  /// the region starting at `b` and continuing up to
  /// `b + self.count - self.count % MemoryLayout<T>.stride` is bound to type `T` and
  /// initialized. If `T` is a nontrivial type, you must eventually deinitialize
  /// or move the values in this region to avoid leaks. If `baseAddress` is
  /// `nil`, this function does nothing and returns an empty buffer pointer.
  ///
  /// - Parameters:
  ///   - type: The type to bind this buffer’s memory to.
  ///   - repeatedValue: The instance to copy into memory.
  /// - Returns: A typed buffer of the memory referenced by this raw buffer.
  ///     The typed buffer contains `self.count / MemoryLayout<T>.stride`
  ///     instances of `T`.
  @discardableResult
  @inlinable
  @_alwaysEmitIntoClient
  func initializeMemory<T>(
    as type: T.Type, repeating repeatedValue: T
  ) -> UnsafeMutableBufferPointer<T> {
    let buffer = Base(rebasing: self)
    return buffer.initializeMemory(as: T.self, repeating: repeatedValue)
  }

  /// Initializes the buffer's memory with the given elements, binding the
  /// initialized memory to the elements' type.
  ///
  /// When calling the `initializeMemory(as:from:)` method on a buffer `b`,
  /// the memory referenced by `b` must be uninitialized or initialized to a
  /// trivial type, and must be properly aligned for accessing `S.Element`.
  /// The buffer must contain sufficient memory to accommodate
  /// `source.underestimatedCount`.
  ///
  /// This method initializes the buffer with elements from `source` until
  /// `source` is exhausted or, if `source` is a sequence but not a
  /// collection, the buffer has no more room for its elements. After calling
  /// `initializeMemory(as:from:)`, the memory referenced by the returned
  /// `UnsafeMutableBufferPointer` instance is bound and initialized to type
  /// `S.Element`.
  ///
  /// - Parameters:
  ///   - type: The type of element to which this buffer's memory will be bound.
  ///   - source: A sequence of elements with which to initialize the buffer.
  /// - Returns: An iterator to any elements of `source` that didn't fit in the
  ///   buffer, and a typed buffer of the written elements. The returned
  ///   buffer references memory starting at the same base address as this
  ///   buffer.
  @inlinable
  @_alwaysEmitIntoClient
  public func initializeMemory<S: Sequence>(
    as type: S.Element.Type, from source: S
  ) -> (unwritten: S.Iterator, initialized: UnsafeMutableBufferPointer<S.Element>) {
    let buffer = Base(rebasing: self)
    return buffer.initializeMemory(as: S.Element.self, from: source)
  }

  /// Initializes the buffer's memory with the given elements, binding the
  /// initialized memory to the elements' type.
  ///
  /// When calling the `initializeMemory(as:fromElements:)` method on a buffer
  /// `b`, the memory referenced by `b` must be uninitialized, or initialized
  /// to a trivial type. `b` must be properly aligned for accessing `C.Element`.
  ///
  /// This method initializes the buffer with the contents of `fromElements`
  /// until `fromElements` is exhausted or the buffer runs out of available
  /// space. After calling `initializeMemory(as:fromElements:)`, the memory
  /// referenced by the returned `UnsafeMutableBufferPointer` instance is bound
  /// and initialized to type `C.Element`. This method does not change
  /// the binding state of the unused portion of `b`, if any.
  ///
  /// - Parameters:
  ///   - type: The type of element to which this buffer's memory will be bound.
  ///   - fromElements: A collection of elements to be used to
  ///     initialize the buffer's storage.
  /// - Returns: A typed buffer of the initialized elements. The returned
  ///   buffer references memory starting at the same base address as this
  ///   buffer, and its count indicates the number of elements copied from
  ///   the collection `elements`.
  @discardableResult
  @inlinable
  @_alwaysEmitIntoClient
  func initializeMemory<C: Collection>(
    as type: C.Element.Type,
    fromElements source: C
  ) -> UnsafeMutableBufferPointer<C.Element> {
    let buffer = Base(rebasing: self)
    return buffer.initializeMemory(as: C.Element.self, fromElements: source)
  }

  /// Moves instances from an initialized source buffer into the
  /// uninitialized memory referenced by this buffer, leaving the source memory
  /// uninitialized and this buffer's memory initialized.
  ///
  /// When calling the `moveInitializeMemory(as:from:)` method on a buffer `b`,
  /// the memory referenced by `b` must be uninitialized, or initialized to a
  /// trivial type. `b` must be properly aligned for accessing `C.Element`.
  ///
  /// The region of memory starting at this pointer and covering
  /// `fromElements.count` instances of the buffer's `Element` type
  /// must be uninitialized, or `Element` must be a trivial type. After
  /// calling `moveInitialize(as:from:)`, the region is initialized and the
  /// memory region underlying `source` is uninitialized.
  ///
  /// - Parameters:
  ///   - type: The type of element to which this buffer's memory will be bound.
  ///   - fromElements: A buffer containing the values to copy.
  ///     The memory region underlying `source` must be initialized.
  ///     The memory regions referenced by `source` and this buffer may overlap.
  /// - Returns: A typed buffer of the initialized elements. The returned
  ///   buffer references memory starting at the same base address as this
  ///   buffer, and its count indicates the number of elements copied from
  ///   `source`.
  @discardableResult
  @inlinable
  @_alwaysEmitIntoClient
  func moveInitializeMemory<T>(
    as type: T.Type,
    fromElements source: UnsafeMutableBufferPointer<T>
  ) -> UnsafeMutableBufferPointer<T> {
    let buffer = Base(rebasing: self)
    return buffer.moveInitializeMemory(as: T.self, fromElements: source)
  }

  /// Moves instances from an initialized source buffer slice into the
  /// uninitialized memory referenced by this buffer, leaving the source memory
  /// uninitialized and this buffer's memory initialized.
  ///
  /// The region of memory starting at this pointer and covering
  /// `fromElements.count` instances of the buffer's `Element` type
  /// must be uninitialized, or `Element` must be a trivial type. After
  /// calling `moveInitialize(as:from:)`, the region is initialized and the
  /// memory region underlying `source[..<source.endIndex]` is uninitialized.
  ///
  /// - Parameters:
  ///   - type: The type of element to which this buffer's memory will be bound.
  ///   - fromElements: A buffer containing the values to copy.
  ///     The memory region underlying `source` must be initialized.
  ///     The memory regions referenced by `source` and this buffer may overlap.
  /// - Returns: A typed buffer of the initialized elements. The returned
  ///   buffer references memory starting at the same base address as this
  ///   buffer, and its count indicates the number of elements copied from
  ///   `source`.
  @discardableResult
  @inlinable
  @_alwaysEmitIntoClient
  func moveInitializeMemory<T>(
    as type: T.Type,
    fromElements source: Slice<UnsafeMutableBufferPointer<T>>
  ) -> UnsafeMutableBufferPointer<T> {
    let buffer = Base(rebasing: self)
    return buffer.moveInitializeMemory(as: T.self, fromElements: source)
  }

  /// Binds this buffer’s memory to the specified type and returns a typed buffer
  /// of the bound memory.
  ///
  /// Use the `bindMemory(to:)` method to bind the memory referenced
  /// by this buffer to the type `T`. The memory must be uninitialized or
  /// initialized to a type that is layout compatible with `T`. If the memory
  /// is uninitialized, it is still uninitialized after being bound to `T`.
  ///
  /// - Warning: A memory location may only be bound to one type at a time. The
  ///   behavior of accessing memory as a type unrelated to its bound type is
  ///   undefined.
  ///
  /// - Parameters:
  ///   - type: The type `T` to bind the memory to.
  /// - Returns: A typed buffer of the newly bound memory. The memory in this
  ///   region is bound to `T`, but has not been modified in any other way.
  ///   The typed buffer references `self.count / MemoryLayout<T>.stride` instances of `T`.
  @discardableResult
  @inlinable
  @_alwaysEmitIntoClient
  public func bindMemory<T>(to type: T.Type) -> UnsafeMutableBufferPointer<T> {
    let buffer = Base(rebasing: self)
    return buffer.bindMemory(to: T.self)
  }

  /// Executes the given closure while temporarily binding the buffer to
  /// instances of type `T`.
  ///
  /// Use this method when you have a buffer to raw memory and you need
  /// to access that memory as instances of a given type `T`. Accessing
  /// memory as a type `T` requires that the memory be bound to that type.
  /// A memory location may only be bound to one type at a time, so accessing
  /// the same memory as an unrelated type without first rebinding the memory
  /// is undefined.
  ///
  /// Any instance of `T` within the re-bound region may be initialized or
  /// uninitialized. The memory underlying any individual instance of `T`
  /// must have the same initialization state (i.e.  initialized or
  /// uninitialized.) Accessing a `T` whose underlying memory
  /// is in a mixed initialization state shall be undefined behaviour.
  ///
  /// If the byte count of the original buffer is not a multiple of
  /// the stride of `T`, then the re-bound buffer is shorter
  /// than the original buffer.
  ///
  /// After executing `body`, this method rebinds memory back to its original
  /// binding state. This can be unbound memory, or bound to a different type.
  ///
  /// - Note: The buffer's base address must match the
  ///   alignment of `T` (as reported by `MemoryLayout<T>.alignment`).
  ///   That is, `Int(bitPattern: self.baseAddress) % MemoryLayout<T>.alignment`
  ///   must equal zero.
  ///
  /// - Note: A raw buffer may represent memory that has been bound to a type.
  ///   If that is the case, then `T` must be layout compatible with the
  ///   type to which the memory has been bound. This requirement does not
  ///   apply if the raw buffer represents memory that has not been bound
  ///   to any type.
  ///
  /// - Parameters:
  ///   - type: The type to temporarily bind the memory referenced by this
  ///     pointer. This pointer must be a multiple of this type's alignment.
  ///   - body: A closure that takes a typed pointer to the
  ///     same memory as this pointer, only bound to type `T`. The closure's
  ///     pointer argument is valid only for the duration of the closure's
  ///     execution. If `body` has a return value, that value is also used as
  ///     the return value for the `withMemoryRebound(to:capacity:_:)` method.
  ///   - buffer: The buffer temporarily bound to instances of `T`.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  @inlinable
  @_alwaysEmitIntoClient
  func withMemoryRebound<T, Result>(
    to type: T.Type, _ body: (UnsafeMutableBufferPointer<T>) throws -> Result
  ) rethrows -> Result {
    let buffer = Base(rebasing: self)
    return try buffer.withMemoryRebound(to: T.self, body)
  }

  /// Returns a typed buffer to the memory referenced by this buffer,
  /// assuming that the memory is already bound to the specified type.
  ///
  /// Use this method when you have a raw buffer to memory that has already
  /// been bound to the specified type. The memory starting at this pointer
  /// must be bound to the type `T`. Accessing memory through the returned
  /// pointer is undefined if the memory has not been bound to `T`. To bind
  /// memory to `T`, use `bindMemory(to:capacity:)` instead of this method.
  ///
  /// - Note: The buffer's base address must match the
  ///   alignment of `T` (as reported by `MemoryLayout<T>.alignment`).
  ///   That is, `Int(bitPattern: self.baseAddress) % MemoryLayout<T>.alignment`
  ///   must equal zero.
  ///
  /// - Parameter to: The type `T` that the memory has already been bound to.
  /// - Returns: A typed pointer to the same memory as this raw pointer.
  @inlinable
  @_alwaysEmitIntoClient
  func assumingMemoryBound<T>(to type: T.Type) -> UnsafeMutableBufferPointer<T> {
    let buffer = Base(rebasing: self)
    return buffer.assumingMemoryBound(to: T.self)
  }
}

extension Slice where Base == UnsafeRawBufferPointer {

  /// Binds this buffer’s memory to the specified type and returns a typed buffer
  /// of the bound memory.
  ///
  /// Use the `bindMemory(to:)` method to bind the memory referenced
  /// by this buffer to the type `T`. The memory must be uninitialized or
  /// initialized to a type that is layout compatible with `T`. If the memory
  /// is uninitialized, it is still uninitialized after being bound to `T`.
  ///
  /// - Warning: A memory location may only be bound to one type at a time. The
  ///   behavior of accessing memory as a type unrelated to its bound type is
  ///   undefined.
  ///
  /// - Parameters:
  ///   - type: The type `T` to bind the memory to.
  /// - Returns: A typed buffer of the newly bound memory. The memory in this
  ///   region is bound to `T`, but has not been modified in any other way.
  ///   The typed buffer references `self.count / MemoryLayout<T>.stride` instances of `T`.
  @discardableResult
  @inlinable
  @_alwaysEmitIntoClient
  public func bindMemory<T>(to type: T.Type) -> UnsafeBufferPointer<T> {
    let buffer = Base(rebasing: self)
    return buffer.bindMemory(to: T.self)
  }

  /// Executes the given closure while temporarily binding the buffer to
  /// instances of type `T`.
  ///
  /// Use this method when you have a buffer to raw memory and you need
  /// to access that memory as instances of a given type `T`. Accessing
  /// memory as a type `T` requires that the memory be bound to that type.
  /// A memory location may only be bound to one type at a time, so accessing
  /// the same memory as an unrelated type without first rebinding the memory
  /// is undefined.
  ///
  /// Any instance of `T` within the re-bound region may be initialized or
  /// uninitialized. The memory underlying any individual instance of `T`
  /// must have the same initialization state (i.e.  initialized or
  /// uninitialized.) Accessing a `T` whose underlying memory
  /// is in a mixed initialization state shall be undefined behaviour.
  ///
  /// If the byte count of the original buffer is not a multiple of
  /// the stride of `T`, then the re-bound buffer is shorter
  /// than the original buffer.
  ///
  /// After executing `body`, this method rebinds memory back to its original
  /// binding state. This can be unbound memory, or bound to a different type.
  ///
  /// - Note: The buffer's base address must match the
  ///   alignment of `T` (as reported by `MemoryLayout<T>.alignment`).
  ///   That is, `Int(bitPattern: self.baseAddress) % MemoryLayout<T>.alignment`
  ///   must equal zero.
  ///
  /// - Note: A raw buffer may represent memory that has been bound to a type.
  ///   If that is the case, then `T` must be layout compatible with the
  ///   type to which the memory has been bound. This requirement does not
  ///   apply if the raw buffer represents memory that has not been bound
  ///   to any type.
  ///
  /// - Parameters:
  ///   - type: The type to temporarily bind the memory referenced by this
  ///     pointer. This pointer must be a multiple of this type's alignment.
  ///   - body: A closure that takes a typed pointer to the
  ///     same memory as this pointer, only bound to type `T`. The closure's
  ///     pointer argument is valid only for the duration of the closure's
  ///     execution. If `body` has a return value, that value is also used as
  ///     the return value for the `withMemoryRebound(to:capacity:_:)` method.
  ///   - buffer: The buffer temporarily bound to instances of `T`.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  @inlinable
  @_alwaysEmitIntoClient
  func withMemoryRebound<T, Result>(
    to type: T.Type, _ body: (UnsafeBufferPointer<T>) throws -> Result
  ) rethrows -> Result {
    let buffer = Base(rebasing: self)
    return try buffer.withMemoryRebound(to: T.self, body)
  }

  /// Returns a typed buffer to the memory referenced by this buffer,
  /// assuming that the memory is already bound to the specified type.
  ///
  /// Use this method when you have a raw buffer to memory that has already
  /// been bound to the specified type. The memory starting at this pointer
  /// must be bound to the type `T`. Accessing memory through the returned
  /// pointer is undefined if the memory has not been bound to `T`. To bind
  /// memory to `T`, use `bindMemory(to:capacity:)` instead of this method.
  ///
  /// - Note: The buffer's base address must match the
  ///   alignment of `T` (as reported by `MemoryLayout<T>.alignment`).
  ///   That is, `Int(bitPattern: self.baseAddress) % MemoryLayout<T>.alignment`
  ///   must equal zero.
  ///
  /// - Parameter to: The type `T` that the memory has already been bound to.
  /// - Returns: A typed pointer to the same memory as this raw pointer.
  @inlinable
  @_alwaysEmitIntoClient
  func assumingMemoryBound<T>(to type: T.Type) -> UnsafeBufferPointer<T> {
    let buffer = Base(rebasing: self)
    return buffer.assumingMemoryBound(to: T.self)
  }
}

extension Slice: _BufferProtocol
  where Base: _BufferProtocol & _RebasableCollection, Base.SubSequence == Self {

  /// Executes the given closure while temporarily binding the memory referenced
  /// by this buffer slice to the given type.
  ///
  /// Use this method when you have a buffer of memory bound to one type and
  /// you need to access that memory as a buffer of another type. Accessing
  /// memory as type `T` requires that the memory be bound to that type. A
  /// memory location may only be bound to one type at a time, so accessing
  /// the same memory as an unrelated type without first rebinding the memory
  /// is undefined.
  ///
  /// The number of instances of `T` referenced by the rebound buffer may be
  /// different than the number of instances of `Element` referenced by the
  /// original buffer slice. The number of instances of `T` will be calculated
  /// at runtime.
  ///
  /// Any instance of `T` within the re-bound region may be initialized or
  /// uninitialized. Every instance of `Pointee` overlapping with a given
  /// instance of `T` should have the same initialization state (i.e.
  /// initialized or uninitialized.) Accessing a `T` whose underlying
  /// `Pointee` storage is in a mixed initialization state shall be
  /// undefined behaviour.
  ///
  /// Because this range of memory is no longer bound to its `Element` type
  /// while the `body` closure executes, do not access memory using the
  /// original buffer slice from within `body`. Instead,
  /// use the `body` closure's buffer argument to access the values
  /// in memory as instances of type `T`.
  ///
  /// After executing `body`, this method rebinds memory back to the original
  /// `Element` type.
  ///
  /// - Note: Only use this method to rebind the buffer's memory to a type
  ///   that is layout compatible with the currently bound `Element` type.
  ///   The stride of the temporary type (`T`) may be an integer multiple
  ///   or a whole fraction of `Element`'s stride.
  ///   To bind a region of memory to a type that does not match these
  ///   requirements, convert the buffer to a raw buffer and use the
  ///   `bindMemory(to:)` method.
  ///   If `T` and `Element` have different alignments, this buffer's
  ///   `baseAddress` must be aligned with the larger of the two alignments.
  ///
  /// - Parameters:
  ///   - type: The type to temporarily bind the memory referenced by this
  ///     buffer. The type `T` must be layout compatible
  ///     with the pointer's `Element` type.
  ///   - body: A closure that takes a typed buffer to the
  ///     same memory as this buffer, only bound to type `T`. The buffer
  ///     parameter contains a number of complete instances of `T` based
  ///     on the capacity of the original buffer and the stride of `Element`.
  ///     The closure's buffer argument is valid only for the duration of the
  ///     closure's execution. If `body` has a return value, that value
  ///     is also used as the return value for the `withMemoryRebound(to:_:)`
  ///     method.
  ///   - buffer: The buffer temporarily bound to `T`.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  @inlinable
  public func withMemoryRebound<T, Result>(
    to type: T.Type, _ body: (UnsafeBufferPointer<T>) throws -> Result
  ) rethrows -> Result {
    let buffer = Base(rebasing: self)
    return try buffer.withMemoryRebound(to: T.self, body)
  }
}

extension Slice: _MutableBufferProtocol
where Base: _MutableBufferProtocol & _RebasableCollection & _MutableBaseAddressProtocol,
      Base.SubSequence == Self {

  /// Initializes every element in this buffer slice's memory to
  /// a copy of the given value.
  ///
  /// The destination memory must be uninitialized or the buffer's `Element`
  /// must be a trivial type. After a call to `initialize(repeating:)`, the
  /// entire region of memory referenced by this buffer slice is initialized.
  ///
  /// - Parameter repeatedValue: The value with which to initialize this
  ///   buffer slice's memory.
  public func initialize(repeating repeatedValue: Base.Element) {
    Base(rebasing: self).initialize(repeating: repeatedValue)
  }

  /// Initializes the buffer slice's memory with the given elements.
  ///
  /// Prior to calling the `initialize(from:)` method on a buffer slice,
  /// the memory it references must be uninitialized,
  /// or the `Element` type must be a trivial type. After the call,
  /// the memory referenced by the buffer slice up to, but not including,
  /// the returned index is initialized.
  /// The buffer must contain sufficient memory to accommodate
  /// `source.underestimatedCount`.
  ///
  /// The returned index is the position of the next uninitialized element
  /// in the buffer slice, which is one past the last element written.
  /// If `source` contains no elements, the returned index is equal to
  /// the buffer's `startIndex`. If `source` contains an equal or greater number
  /// of elements than the buffer slice can hold, the returned index is equal to
  /// the buffer's `endIndex`.
  ///
  /// - Parameter source: A sequence of elements with which to initialize the
  ///   buffer.
  /// - Returns: An iterator to any elements of `source` that didn't fit in the
  ///   buffer, and an index to the next uninitialized element in the buffer.
  public func initialize<S>(
    from source: S
  ) -> (S.Iterator, Index) where S: Sequence, Base.Element == S.Element {
    let buffer = Base(rebasing: self)
    let (iterator, index) = buffer.initialize(from: source)
    let distance = buffer.distance(from: buffer.startIndex, to: index)
    return (iterator, startIndex.advanced(by: distance))
  }

  /// Initializes the buffer slice's memory with the given elements.
  ///
  /// Prior to calling the `initialize(fromElements:)` method on a buffer slice,
  /// the memory it references must be uninitialized,
  /// or the `Element` type must be a trivial type. After the call,
  /// the memory referenced by the buffer slice up to, but not including,
  /// the returned index is initialized.
  ///
  /// The returned index is the position of the next uninitialized element
  /// in the buffer slice, which is one past the last element written.
  /// If `fromElements` contains no elements, the returned index is equal to
  /// the buffer's `startIndex`. If `fromElements` contains an equal or greater
  /// of elements than the buffer slice can hold, the returned index is equal to
  /// to the buffer's `endIndex`.
  ///
  /// - Parameter fromElements: A collection of elements to be used to
  ///   initialize the buffer's storage.
  /// - Returns: An index to the next uninitialized element in the buffer,
  ///   or `endIndex`.
  @discardableResult
  public func initialize<C>(
    fromElements source: C
  ) -> Int where C : Collection, Base.Element == C.Element {
    let buffer = Base(rebasing: self)
    let index = buffer.initialize(fromElements: source)
    let distance = buffer.distance(from: buffer.startIndex, to: index)
    return startIndex.advanced(by: distance)
  }

  /// Updates every element of this buffer slice's initialized memory.
  ///
  /// The buffer slice’s memory must be initialized or its `Element`
  /// must be a trivial type.
  ///
  /// - Note: All buffer elements must already be initialized.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value used when updating this pointer's memory.
  public func update(repeating repeatedValue: Base.Element) {
    Base(rebasing: self).update(repeating: repeatedValue)
  }

  /// Updates the buffer slice's initialized memory with the given elements.
  ///
  /// The buffer slice's memory must be initialized or its `Element` type
  /// must be a trivial type.
  ///
  /// - Parameter source: A sequence of elements to be used to update
  ///   the buffer's contents.
  /// - Returns: An iterator to any elements of `source` that didn't fit in the
  ///   buffer, and the index one past the last updated element in the buffer.
  public func update<S: Sequence>(
    from source: S
  ) -> (unwritten: S.Iterator, updated: Index) where S.Element == Element {
    let buffer = Base(rebasing: self)
    let (iterator, index) = buffer.update(from: source)
    let distance = buffer.distance(from: buffer.startIndex, to: index)
    return (iterator, startIndex.advanced(by: distance))
  }

  /// Updates the buffer slice's initialized memory with the given elements.
  ///
  /// The buffer slice's memory must be initialized or the buffer's `Element` type
  /// must be a trivial type.
  ///
  /// - Parameter fromElements: A collection of elements to be used to update
  ///   the buffer's contents.
  /// - Returns: An index one past the last updated element in the buffer,
  ///   or `endIndex`.
  @discardableResult
  public func update<C: Collection>(
    fromElements source: C
  ) -> Index where Base.Element == C.Element {
    let buffer = Base(rebasing: self)
    let index = buffer.update(fromElements: source)
    let distance = buffer.distance(from: buffer.startIndex, to: index)
    return startIndex.advanced(by: distance)
  }

  /// Moves every element of an initialized source buffer into the
  /// uninitialized memory referenced by this buffer slice, leaving the
  /// source memory uninitialized and this buffer slice's memory initialized.
  ///
  /// The region of memory starting at the beginning of this buffer and
  /// covering `source.count` instances of its `Element` type must be
  /// uninitialized, or `Element` must be a trivial type. After calling
  /// `moveInitialize(fromElements:)`, the region is initialized and
  /// the region of memory underlying `source` is uninitialized.
  ///
  /// - Parameter source: A buffer containing the values to copy. The memory
  ///   region underlying `source` must be initialized. The memory regions
  ///   referenced by `source` and this buffer may overlap.
  /// - Returns: An index to the next uninitialized element in the buffer,
  ///   or `endIndex`.
  @discardableResult
  public func moveInitialize(
    fromElements source: UnsafeMutableBufferPointer<Base.Element>
  ) -> Index {
    let buffer = Base(rebasing: self)
    let index = buffer.moveInitialize(fromElements: source)
    let distance = buffer.distance(from: buffer.startIndex, to: index)
    return startIndex.advanced(by: distance)
  }

  /// Moves every element of an initialized source buffer slice into the
  /// uninitialized memory referenced by this buffer slice, leaving the
  /// source memory uninitialized and this buffer slice's memory initialized.
  ///
  /// The region of memory starting at the beginning of this buffer slice and
  /// covering `source.count` instances of its `Element` type must be
  /// uninitialized, or `Element` must be a trivial type. After calling
  /// `moveInitialize(fromElements:)`, the region is initialized and
  /// the region of memory underlying `source` is uninitialized.
  ///
  /// - Parameter source: A buffer containing the values to copy. The memory
  ///   region underlying `source` must be initialized. The memory regions
  ///   referenced by `source` and this buffer may overlap.
  /// - Returns: An index one past the last replaced element in the buffer,
  ///   or `endIndex`.
  @discardableResult
  public func moveInitialize(
    fromElements source: Slice<UnsafeMutableBufferPointer<Base.Element>>
  ) -> Index {
    let buffer = Base(rebasing: self)
    let index = buffer.moveInitialize(fromElements: source)
    let distance = buffer.distance(from: buffer.startIndex, to: index)
    return startIndex.advanced(by: distance)
  }

  /// Updates this buffer slice's initialized memory initialized memory by
  /// moving every element from the source buffer,
  /// leaving the source memory uninitialized.
  ///
  /// The region of memory starting at the beginning of this buffer slice and
  /// covering `fromElements.count` instances of its `Element` type  must be
  /// initialized, or `Element` must be a trivial type. After calling
  /// `moveUpdate(fromElements:)`,
  /// the region of memory underlying `source` is uninitialized.
  ///
  /// - Parameter source: A buffer containing the values to move.
  ///   The memory region underlying `source` must be initialized. The
  ///   memory regions referenced by `source` and this pointer must not overlap.
  /// - Returns: An index one past the last updated element in the buffer,
  ///   or `endIndex`.
  @discardableResult
  public func moveUpdate(
    fromElements source: UnsafeMutableBufferPointer<Base.Element>
  ) -> Index {
    let buffer = Base(rebasing: self)
    let index = buffer.moveUpdate(fromElements: source)
    let distance = buffer.distance(from: buffer.startIndex, to: index)
    return startIndex.advanced(by: distance)
  }

  /// Updates this buffer slice's initialized memory initialized memory by
  /// moving every element from the source buffer slice,
  /// leaving the source memory uninitialized.
  ///
  /// The region of memory starting at the beginning of this buffer slice and
  /// covering `fromElements.count` instances of its `Element` type  must be
  /// initialized, or `Element` must be a trivial type. After calling
  /// `moveUpdate(fromElements:)`,
  /// the region of memory underlying `source` is uninitialized.
  ///
  /// - Parameter source: A buffer containing the values to move.
  ///   The memory region underlying `source` must be initialized. The
  ///   memory regions referenced by `source` and this pointer must not overlap.
  /// - Returns: An index one past the last updated element in the buffer,
  ///   or `endIndex`.
  @discardableResult
  public func moveUpdate(
    fromElements source: Slice<UnsafeMutableBufferPointer<Base.Element>>
  ) -> Index {
    let buffer = Base(rebasing: self)
    let index = buffer.moveUpdate(fromElements: source)
    let distance = buffer.distance(from: buffer.startIndex, to: index)
    return startIndex.advanced(by: distance)
  }

  /// Deinitializes every instance in this buffer slice.
  ///
  /// The region of memory underlying this buffer slice must be fully
  /// initialized. After calling `deinitialize(count:)`, the memory
  /// is uninitialized, but still bound to the `Element` type.
  ///
  /// - Note: All buffer elements must already be initialized.
  ///
  /// - Returns: A raw buffer to the same range of memory as this buffer.
  ///   The range of memory is still bound to `Element`.
  @discardableResult
  public func deinitialize() -> UnsafeMutableRawBufferPointer {
    Base(rebasing: self).deinitialize()
  }

  /// Initializes the element at `index` to the given value.
  ///
  /// The memory underlying the destination element must be uninitialized,
  /// or `Element` must be a trivial type. After a call to `initialize(to:)`,
  /// the memory underlying this element of the buffer slice is initialized.
  ///
  /// - Parameters:
  ///   - value: The value used to initialize the buffer element's memory.
  ///   - index: The index of the element to initialize
  public func initializeElement(at index: Int, to value: Element) {
    assert(startIndex <= index && index < endIndex)
    base.baseAddress.unsafelyUnwrapped.advanced(by: index).initialize(to: value)
  }

  /// Updates the initialized element at `index` to the given value.
  ///
  /// The memory underlying the destination element must be initialized,
  /// or `Element` must be a trivial type. This method is equivalent to:
  ///
  ///     self[index] = value
  ///
  /// - Parameters:
  ///   - value: The value used to update the buffer element's memory.
  ///   - index: The index of the element to update
  public func updateElement(at index: Index, to value: Element) {
    assert(startIndex <= index && index < endIndex)
    base.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee = value
  }

  /// Retrieves and returns the element at `index`,
  /// leaving that element's underlying memory uninitialized.
  ///
  /// The memory underlying the element at `index` must be initialized.
  /// After calling `moveElement(from:)`, the memory underlying this element
  /// of the buffer slice is uninitialized, and still bound to type `Element`.
  ///
  /// - Parameters:
  ///   - index: The index of the buffer element to retrieve and deinitialize.
  /// - Returns: The instance referenced by this index in this buffer.
  public func moveElement(from index: Index) -> Element {
    assert(startIndex <= index && index < endIndex)
    return base.baseAddress.unsafelyUnwrapped.advanced(by: index).move()
  }

  /// Deinitializes the memory underlying the element at `index`.
  ///
  /// The memory underlying the element at `index` must be initialized.
  /// After calling `deinitializeElement()`, the memory underlying this element
  /// of the buffer slice is uninitialized, and still bound to type `Element`.
  ///
  /// - Parameters:
  ///   - index: The index of the buffer element to deinitialize.
  public func deinitializeElement(at index: Base.Index) {
    assert(startIndex <= index && index < endIndex)
    base.baseAddress.unsafelyUnwrapped.advanced(by: index).deinitialize(count: 1)
  }

  /// Executes the given closure while temporarily binding the memory referenced
  /// by this buffer slice to the given type.
  ///
  /// Use this method when you have a buffer of memory bound to one type and
  /// you need to access that memory as a buffer of another type. Accessing
  /// memory as type `T` requires that the memory be bound to that type. A
  /// memory location may only be bound to one type at a time, so accessing
  /// the same memory as an unrelated type without first rebinding the memory
  /// is undefined.
  ///
  /// The number of instances of `T` referenced by the rebound buffer may be
  /// different than the number of instances of `Element` referenced by the
  /// original buffer slice. The number of instances of `T` will be calculated
  /// at runtime.
  ///
  /// Any instance of `T` within the re-bound region may be initialized or
  /// uninitialized. Every instance of `Pointee` overlapping with a given
  /// instance of `T` should have the same initialization state (i.e.
  /// initialized or uninitialized.) Accessing a `T` whose underlying
  /// `Pointee` storage is in a mixed initialization state shall be
  /// undefined behaviour.
  ///
  /// Because this range of memory is no longer bound to its `Element` type
  /// while the `body` closure executes, do not access memory using the
  /// original buffer slice from within `body`. Instead,
  /// use the `body` closure's buffer argument to access the values
  /// in memory as instances of type `T`.
  ///
  /// After executing `body`, this method rebinds memory back to the original
  /// `Element` type.
  ///
  /// - Note: Only use this method to rebind the buffer's memory to a type
  ///   that is layout compatible with the currently bound `Element` type.
  ///   The stride of the temporary type (`T`) may be an integer multiple
  ///   or a whole fraction of `Element`'s stride.
  ///   To bind a region of memory to a type that does not match these
  ///   requirements, convert the buffer to a raw buffer and use the
  ///   `bindMemory(to:)` method.
  ///   If `T` and `Element` have different alignments, this buffer's
  ///   `baseAddress` must be aligned with the larger of the two alignments.
  ///
  /// - Parameters:
  ///   - type: The type to temporarily bind the memory referenced by this
  ///     buffer. The type `T` must be layout compatible
  ///     with the pointer's `Element` type.
  ///   - body: A closure that takes a ${Mutable.lower()} typed buffer to the
  ///     same memory as this buffer, only bound to type `T`. The buffer
  ///     parameter contains a number of complete instances of `T` based
  ///     on the capacity of the original buffer and the stride of `Element`.
  ///     The closure's buffer argument is valid only for the duration of the
  ///     closure's execution. If `body` has a return value, that value
  ///     is also used as the return value for the `withMemoryRebound(to:_:)`
  ///     method.
  ///   - buffer: The buffer temporarily bound to `T`.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  @inlinable
  public func withMemoryRebound<T, Result>(
    to type: T.Type, _ body: (UnsafeMutableBufferPointer<T>) throws -> Result
  ) rethrows -> Result {
    try Base(rebasing: self).withMemoryRebound(to: T.self, body)
  }

  @inlinable
  func withContiguousMutableStorageIfAvailable<R>(
    _ body: (_ buffer: inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    try base.withContiguousStorageIfAvailable { buffer in
      let start = base.baseAddress?.advanced(by: startIndex)
      var slice = UnsafeMutableBufferPointer(start: start, count: count)
      let (b,c) = (slice.baseAddress, slice.count)
      defer {
        precondition(
          slice.baseAddress == b && slice.count == c,
          "Slice.withContiguousMutableStorageIfAvailable: " +
          "replacing the buffer is not allowed")
      }
      return try body(&slice)
    }
  }
}
