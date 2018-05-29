//===--- SentinelCollection.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
public // @testable
protocol _Function {
  associatedtype Input
  associatedtype Output
  func apply(_: Input) -> Output
}

protocol _Predicate : _Function where Output == Bool { }

@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal struct _SentinelIterator<
  Base: IteratorProtocol, 
  IsSentinel : _Predicate
> : IteratorProtocol, Sequence
where IsSentinel.Input == Base.Element {
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _base: Base
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _isSentinel: IsSentinel
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _expired: Bool = false

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ base: Base, until condition: IsSentinel) {
    _base = base
    _isSentinel = condition
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func next() -> Base.Element? {
    guard _fastPath(!_expired) else { return nil }
    let x = _base.next()
    // We don't need this check if it's a precondition that the sentinel will be
    // found
    // guard _fastPath(x != nil), let y = x else { return x }
    guard _fastPath(!_isSentinel.apply(x!)) else { _expired = true; return nil }
    return x
  }
}

@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal struct _SentinelCollection<
  Base: Collection, 
  IsSentinel : _Predicate
> : Collection
where IsSentinel.Input == Base.Iterator.Element {
  @usableFromInline // FIXME(sil-serialize-all)
  internal let _isSentinel: IsSentinel
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _base : Base
  
  @inlinable // FIXME(sil-serialize-all)
  internal func makeIterator() -> _SentinelIterator<Base.Iterator, IsSentinel> {
    return _SentinelIterator(_base.makeIterator(), until: _isSentinel)
  }
  
  @_fixed_layout // FIXME(sil-serialize-all)
  @usableFromInline // FIXME(sil-serialize-all)
  internal struct Index : Comparable {
    @inlinable // FIXME(sil-serialize-all)
    internal init(
      _impl: (position: Base.Index, element: Base.Iterator.Element)?
    ) {
      self._impl = _impl
    }

    @usableFromInline // FIXME(sil-serialize-all)
    internal var _impl: (position: Base.Index, element: Base.Iterator.Element)?

    @inlinable // FIXME(sil-serialize-all)
    internal static func == (lhs: Index, rhs: Index) -> Bool {
      if rhs._impl == nil { return lhs._impl == nil }
      return lhs._impl != nil && rhs._impl!.position == lhs._impl!.position
    }

    @inlinable // FIXME(sil-serialize-all)
    internal static func < (lhs: Index, rhs: Index) -> Bool {
      if rhs._impl == nil { return lhs._impl != nil }
      return lhs._impl != nil && rhs._impl!.position < lhs._impl!.position
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var startIndex : Index {
    return _index(at: _base.startIndex)
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal var endIndex : Index {
    return Index(_impl: nil)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal subscript(i: Index) -> Base.Iterator.Element {
    return i._impl!.element
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(after i: Index) -> Index {
    return _index(at: _base.index(after: i._impl!.position))
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _index(at i: Base.Index) -> Index {
    // We don't need this check if it's a precondition that the sentinel will be
    // found
    // guard _fastPath(i != _base.endIndex) else { return endIndex }
    let e = _base[i]
    guard _fastPath(!_isSentinel.apply(e)) else { return endIndex }
    return Index(_impl: (position: i, element: e))
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal init(_ base: Base, until condition: IsSentinel) {
    _base = base
    _isSentinel = condition
  }
}

@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal struct _IsZero<T : BinaryInteger> : _Predicate {
  @inlinable // FIXME(sil-serialize-all)
  internal init() {}

  @inlinable // FIXME(sil-serialize-all)
  internal func apply(_ x: T) -> Bool {
    return x == 0
  }
}
