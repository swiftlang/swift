
public typealias ArraySlice<T> = Slice<[T]>

extension Slice: _ContiguouslyStored where Base: _ContiguouslyStored
/* FIXME: exterminate */, Base.IndexDistance == Int 
{
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let i = _base.distance(from: _base.startIndex, to: _startIndex)
    let j = _base.distance(from: _base.startIndex, to: _endIndex)
    return try _base.withUnsafeBufferPointer { buf in
      let sliced = UnsafeBufferPointer(rebasing: buf[i..<j])
      return try body(sliced)
    }
  }

  public mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let i = _base.distance(from: _base.startIndex, to: _startIndex)
    let j = _base.distance(from: _base.startIndex, to: _endIndex)
    return try _base.withUnsafeMutableBufferPointer { buf in
      var sliced = UnsafeMutableBufferPointer(rebasing: buf[i..<j])
      return try body(&sliced)
    }
  }

  @_inlineable
  public mutating func withUnsafeMutableBytes<R>(
    _ body: (UnsafeMutableRawBufferPointer) throws -> R
  ) rethrows -> R {
    return try self.withUnsafeMutableBufferPointer {
      return try body(UnsafeMutableRawBufferPointer($0))
    }
  }
  @_inlineable
  public func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    return try self.withUnsafeBufferPointer {
      try body(UnsafeRawBufferPointer($0))
    }
  }
}

extension Slice: Equatable where Base: _ContiguouslyStored, Base.Element: Equatable {
  public static func == (lhs: Slice<Base>, rhs: Slice<Base>) -> Bool {
    return lhs.elementsEqual(rhs)
  }
}

// %if Self == 'ArraySlice':
// /// Initialization from an existing buffer does not have "array.init"
// /// semantics because the caller may retain an alias to buffer.
// @_inlineable
// @_versioned
// internal init(_buffer buffer: _ContiguousArrayBuffer<Element>) {
//   self.init(_buffer: _Buffer(_buffer: buffer, shiftedToStartIndex: 0))
// }
// %end
//
//   @_inlineable // FIXME(sil-serialize-all)
//   public var endIndex: Int {
// %if Self == 'ArraySlice':
//     return _buffer.endIndex
// %else:
//
// %if Self == 'ArraySlice':
//   /// Removes and returns the last element of the array.
//   ///
//   /// The array must not be empty. This example removes the last number from an
//   /// array of `Double` values.
//   ///
//   ///     var measurements: [Double] = [1.1, 1.5, 2.9, 1.2, 1.5, 1.3, 1.2]
//   ///     let removed = measurements.removeLast()
//   ///     print(measurements)
//   ///     // Prints "[1.1, 1.5, 2.9, 1.2, 1.5, 1.3]"
//   ///
//   /// - Returns: The element that was removed.
//   @_inlineable
//   public mutating func _customRemoveLast() -> Element? {
//     _precondition(count > 0, "Can't removeLast from an empty ${Self}")
//     // FIXME(performance): if `self` is uniquely referenced, we should remove
//     // the element as shown below (this will deallocate the element and
//     // decrease memory use).  If `self` is not uniquely referenced, the code
//     // below will make a copy of the storage, which is wasteful.  Instead, we
//     // should just shrink the view without allocating new storage.
//     let i = endIndex
//     // We don't check for overflow in `i - 1` because `i` is known to be
//     // positive.
//     let result = self[i &- 1]
//     self.replaceSubrange((i &- 1)..<i, with: EmptyCollection())
//     return result
//   }
// %end
//
// Equatable:
// %if Self == 'ArraySlice':
//
//   var streamLHS = lhs.makeIterator()
//   var streamRHS = rhs.makeIterator()
//
//   var nextLHS = streamLHS.next()
//   while nextLHS != nil {
//     let nextRHS = streamRHS.next()
//     if nextLHS != nextRHS {
//       return false
//     }
//     nextLHS = streamLHS.next()
//   }
//
//   extension ArraySlice {
//     @_inlineable
//     public // @testable
//     init(_startIndex: Int) {
//       self.init(
//         _buffer: _Buffer(
//           _buffer: ContiguousArray()._buffer,
//           shiftedToStartIndex: _startIndex))
//     }
//   }
//
//
//   %if Self == 'ArraySlice':
//     /// The position of the first element in a nonempty array.
//     ///
//     /// If the array is empty, `startIndex` is equal to `endIndex`.
//   %else:
//
//   public var startIndex: Int {
// %if Self == 'ArraySlice':
//     return _buffer.startIndex
// %else:
//     return 0
// %end
//   }
