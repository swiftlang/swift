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
struct _SentinelIterator<
  Base: IteratorProtocol, 
  IsSentinel : _Predicate
> : IteratorProtocol, Sequence
where IsSentinel.Input == Base.Element {
  var _base: Base
  var _isSentinel: IsSentinel
  var _expired: Bool = false

  init(_ base: Base, until condition: IsSentinel) {
    _base = base
    _isSentinel = condition
  }
  
  mutating func next() -> Base.Element? {
    guard _fastPath(!_expired) else { return nil }
    let x = _base.next()
    // We don't need this check if it's a precondition that the sentinel will be
    // found
    // guard _fastPath(x != nil), let y = x else { return x }
    guard _fastPath(!_isSentinel.apply(x!)) else { _expired = true; return nil }
    return x
  }
}

struct _SentinelCollection<
  Base: Collection, 
  IsSentinel : _Predicate
> : Collection
where IsSentinel.Input == Base.Iterator.Element {
  let _isSentinel: IsSentinel
  var _base : Base
  
  typealias IndexDistance = Base.IndexDistance

  func makeIterator() -> _SentinelIterator<Base.Iterator, IsSentinel> {
    return _SentinelIterator(_base.makeIterator(), until: _isSentinel)
  }
  
  struct Index : Comparable {
    var _impl: (position: Base.Index, element: Base.Iterator.Element)?

    static func == (lhs: Index, rhs: Index) -> Bool {
      if rhs._impl == nil { return lhs._impl == nil }
      return lhs._impl != nil && rhs._impl!.position == lhs._impl!.position
    }

    static func < (lhs: Index, rhs: Index) -> Bool {
      if rhs._impl == nil { return lhs._impl != nil }
      return lhs._impl != nil && rhs._impl!.position < lhs._impl!.position
    }
  }

  var startIndex : Index {
    return _index(at: _base.startIndex)
  }
  
  var endIndex : Index {
    return Index(_impl: nil)
  }

  subscript(i: Index) -> Base.Iterator.Element {
    return i._impl!.element
  }

  func index(after i: Index) -> Index {
    return _index(at: _base.index(after: i._impl!.position))
  }

  func _index(at i: Base.Index) -> Index {
    // We don't need this check if it's a precondition that the sentinel will be
    // found
    // guard _fastPath(i != _base.endIndex) else { return endIndex }
    let e = _base[i]
    guard _fastPath(!_isSentinel.apply(e)) else { return endIndex }
    return Index(_impl: (position: i, element: e))
  }
  
  init(_ base: Base, until condition: IsSentinel) {
    _base = base
    _isSentinel = condition
  }
}

struct _IsZero<T : BinaryInteger> : _Predicate {
  func apply(_ x: T) -> Bool {
    return x == 0
  }
}
