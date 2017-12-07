
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

extension Slice: ExpressibleByArrayLiteral 
  where Base: ExpressibleByArrayLiteral, Base.ArrayLiteralElement == Base.Element {
  public typealias ArrayLiteralElement = Element
  public init(arrayLiteral elements: ArrayLiteralElement...) {
    let base = Base(arrayLiteral: elements)
    self = Slice(base: base, bounds: base.startIndex..<base.endIndex)
  }
}

extension Slice where Base: RangeReplaceableCollection {
  @_inlineable
  @available(swift, introduced: 4.0)
  public func filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> Base {
    return try Base(base[_startIndex..<_endIndex].lazy.filter(isIncluded))
  }
}
