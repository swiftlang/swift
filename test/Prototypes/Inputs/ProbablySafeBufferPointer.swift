#if canImport(Darwin)
import Darwin

internal func _getCurrentThreadStackRange() -> Range<UnsafeRawPointer> {
  let currentThread = pthread_self()
  let stackAddr = UnsafeRawPointer(pthread_get_stackaddr_np(currentThread))
  let stackSize = pthread_get_stacksize_np(currentThread)
  return (stackAddr - stackSize)..<stackAddr
}

internal enum _ProbablySafeBufferPointerCheckInfo {
  case empty
  case `static`
  case stack(Range<UnsafeRawPointer>)
  case heap(UnsafeRawBufferPointer)

  internal init(for address: UnsafeRawPointer) {
    var unused = dl_info()
    if dladdr(address, &unused) != 0 {
      self = .static
      return
    }

    let stackRange = _getCurrentThreadStackRange()
    if stackRange.contains(address) {
      self = .stack(stackRange)
      return
    }

    // See if this address is part of a heap allocation by walking backwards
    // until we get to the start of an allocation...but stop when we hit the
    // start of the page, because we don't want to accidentally walk onto
    // another page. That would probably cause the program to trap, and for the
    // wrong reason. Probably.
    let pageAddressBitPattern = UInt(bitPattern: address) & ~vm_page_mask
    let pageAddress = UnsafeRawPointer(bitPattern: pageAddressBitPattern)!
    for startAddress in (pageAddress...address).reversed() {
      if malloc_zone_from_ptr(startAddress) != nil {
        let allocationSize = malloc_size(startAddress)
        precondition(address - startAddress < allocationSize,
                     "address in malloc space is out of bounds")
        self = .heap(UnsafeRawBufferPointer(start: startAddress,
                                            count: allocationSize))
        return
      }
    }

    // FIXME: This doesn't support mmap, or even very large malloc allocations.
    // But people probably won't try to use ProbablySafeBufferPointer with
    // those. Probably.
    preconditionFailure("this address is probably not safe!")
  }

  internal func canProbablyAccess(_ address: UnsafeRawPointer) -> Bool {
    switch self {
    case .empty:
      fatalError("you can't access anything")
    case .static:
      // Unfortunately we can't bound-check static data. But it's probably safe.
      // Probably.
      return true
    case .stack(let range):
      // We can't bounds-check the stack either---it changes too fast, and
      // besides, Swift doesn't give us any way to get the *current* top of the
      // stack. But it's probably safe. Probably.
      return range.contains(address)
    case .heap(let fullAllocation):
      return address > fullAllocation.baseAddress! &&
             address - fullAllocation.baseAddress! < fullAllocation.count
    }
  }

  internal func isProbablyStillValid() -> Bool {
    switch self {
    case .empty, .static:
      return true
    case .stack(let range):
      // Check if we've moved to a different thread.
      return range == _getCurrentThreadStackRange()
    case .heap(let fullAllocation):
      guard malloc_zone_from_ptr(fullAllocation.baseAddress) != nil else {
        // This allocation has been deallocated.
        return false
      }
      // If the counts match up, this allocation *probably* hasn't been
      // deallocated and replaced with another of the same size. Probably.
      return malloc_size(fullAllocation.baseAddress) == fullAllocation.count
    }
  }
}

/// A nonowning collection interface to a buffer of elements stored contiguously
/// in memory where accesses are checked to make sure they are safe. Probably
/// safe.
///
/// ProbablySafeBufferPointer Semantics
/// ===================================
///
/// A `ProbablySafeBufferPointer` instance is a view into memory and does not
/// own the memory that it references. Copying a value of type
/// `ProbablySafeBufferPointer` does not copy the instances stored in the
/// underlying memory. However, initializing another collection with a
/// `ProbablySafeBufferPointer` instance copies the instances out of the
/// referenced memory and into the new collection.
///
/// By the way, sharing data across threads is probably unsafe, so
/// ProbablySafeBufferPointer won't let you do it.
public struct ProbablySafeBufferPointer<Element> {
  internal var _buffer: UnsafeBufferPointer<Element>
  internal var _checkInfo: _ProbablySafeBufferPointerCheckInfo

  /// Creates a probably safe buffer pointer referencing the same memory as the 
  /// given unsafe buffer pointer.
  ///
  /// This does check whether it's safe to access the entire buffer, aborting
  /// the program if it is not.
  ///
  /// - Parameter buffer: The unsafe buffer pointer to convert.
  public init(_ buffer: UnsafeBufferPointer<Element>) {
    precondition(buffer.count >= 0)
    self._buffer = buffer

    if buffer.count == 0 {
      self._checkInfo = .empty
    } else {
      self._checkInfo = .init(for: buffer.baseAddress!)
      let lastAddressInBuffer = buffer.baseAddress! + buffer.count - 1
      precondition(self._checkInfo.canProbablyAccess(lastAddressInBuffer),
                   "end of buffer is outside of allocation")
    }
  }

  /// Creates a probably safe buffer pointer referencing the same memory as the 
  /// given unsafe buffer pointer.
  ///
  /// This does check whether it's safe to access the entire buffer, aborting
  /// the program if it is not.
  ///
  /// - Parameter buffer: The unsafe buffer pointer to convert.
  public init(_ buffer: UnsafeMutableBufferPointer<Element>) {
    self.init(UnsafeBufferPointer(buffer))
  }

  /// Creates a new buffer pointer over the specified number of contiguous
  /// instances beginning at the given pointer.
  ///
  /// This does check whether it's safe to access the entire buffer, aborting
  /// the program if it is not.
  ///
  /// - Parameters:
  ///   - start: A pointer to the start of the buffer, or `nil`. If `start` is
  ///     `nil`, `count` must be zero. However, `count` may be zero even for a
  ///     non-`nil` `start`. The pointer passed as `start` must be aligned to
  ///     `MemoryLayout<Element>.alignment`.
  ///   - count: The number of instances in the buffer. `count` must not be
  ///     negative.
  public init(start: UnsafePointer<Element>?, count: Int) {
    self.init(UnsafeBufferPointer(start: start, count: count))
  }
  
  /// Accesses the element at the specified position, after checking that the
  /// buffer is still safe to access. Probably.
  ///
  /// - Parameter index: The position of the element to access. `index` must be
  ///   in the range `0..<count`.
  public subscript(index: Int) -> Element {
    precondition(index >= 0, "index must be non-negative")
    precondition(index < self._buffer.count, "index is out of bounds")
    precondition(self._checkInfo.isProbablyStillValid(),
                 "allocation is no longer valid. probably.")
    return self._buffer[index]
  }
  
  /// Calls a closure with a pointer to the array's contiguous storage.
  ///
  /// - Parameter body: A closure with an `UnsafeBufferPointer` parameter that
  ///   points to the contiguous storage for the array. If `body` has a return
  ///   value, that value is also used as the return value for the
  ///   `withUnsafeBufferPointer(_:)` method. The pointer argument is valid only
  ///   for the duration of the method's execution.
  /// - Returns: The return value, if any, of the `body` closure parameter.  
  public func withUnsafeBufferPointer<Result>(
    _ body: (UnsafeBufferPointer<Element>) throws -> Result
  ) rethrows -> Result {
    precondition(self._checkInfo.isProbablyStillValid(),
                 "allocation is no longer valid. probably.")
    return try body(self._buffer)
  }

  /// Deallocates the memory block previously allocated at this buffer pointer’s 
  /// base address. 
  ///
  /// This buffer pointer's base address must be `nil` or a pointer to a memory 
  /// block previously returned by a Swift allocation method. If `baseAddress`
  /// is `nil`, this function does nothing. Otherwise, the memory must not be
  /// initialized  or `Element` must be a trivial type. This buffer pointer's
  /// `count` must be equal to the originally allocated size of the memory
  /// block.
  public func deallocate() {
    // The underlying memory management will probably validate this for us.
    // Probably.
    self._buffer.deallocate()
  }
}

extension ProbablySafeBufferPointer: RandomAccessCollection {
  /// The index of the first element in a nonempty buffer.
  ///
  /// The `startIndex` property of a `ProbablySafeBufferPointer` instance is
  /// always zero.
  public var startIndex: Int { return self._buffer.startIndex }

  /// The "past the end" position---that is, the position one greater than the
  /// last valid subscript argument.
  ///
  /// The `endIndex` property of a `ProbablySafeBufferPointer` instance is
  /// always identical to `count`.
  public var endIndex: Int { return self._buffer.endIndex }
}

#else
// FIXME: Patches welcome to support non-Apple platforms!
@available(*, unavailable,
           message: "this content is not available in your region")
public struct ProbablySafeBufferPointer<Element> {}
#endif
