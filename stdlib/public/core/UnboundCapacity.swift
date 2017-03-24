//===--- UnboundCapacity.swift --------------------------------------------===//
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
// Create a wrapper around a buffer of bounded size that falls back to using an
// Array when the size grows past the underlying buffer's maximum capacity
// ===----------------------------------------------------------------------===//

/// Augments a base collection, which may have limited capacity, with heap
/// storage, such that the result has unbounded capacity.
enum UnboundCapacity<
  Base: RandomAccessCollection & MutableCollection & _BoundedCollection
> {
  case small(Base)
  case large([Base.Iterator.Element])
}

extension UnboundCapacity : MutableCollection {
  public typealias Index = Int
  public var startIndex : Int { return 0 }
  
  public var endIndex : Int {
    switch self {
    case .small(let base):
      return numericCast(base.count)
    case .large(let base):
      return base.count
    }
  }

  public subscript(i: Index) -> Base.Iterator.Element {
    @inline(__always)
    get {
      switch self {
      case .small(let base):
        return base[base.index(atOffset: i)]
      case .large(let base):
        return base[base.index(atOffset: i)]
      }
    }
    set {
      switch self {
      case .small(var base):
        base[base.index(atOffset: i)] = newValue
      case .large(_):
        withMutableArray {
          $0[i] = newValue
        }
      }
    }
  }
  
  public func index(after i: Index) -> Index {
    return i + 1
  }
}

extension UnboundCapacity : BidirectionalCollection {
  public func index(before i: Index) -> Index {
    return i - 1
  }
}

extension UnboundCapacity : RandomAccessCollection {
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return i + n
  }
  public func distance(from i: Index, to j: Index) -> Int {
    return j - i
  }
}

/// Helper methods
extension UnboundCapacity {

  /// Converts our contents into an array with at least the given
  /// capacity, and presents it to `body` for mutation.
  //
  // Michael NOTE: forwarding version added due to odd linker behavior. Linker
  // couldn't find the version with a default argument. This is just for the
  // prototype, we should remove and restore the default parameter.
  //
  // Michael NOTE: I'll also be dropping the use of this in favor of adding a
  // withUnsafeMutableBufferPointer.
  internal mutating func withMutableArray<R>(
    body: (inout [Base.Iterator.Element]) throws->R
  ) rethrows -> R {
    return try withMutableArray(minCapacity: 0, body: body)
  }
  internal mutating func withMutableArray<R>(
    minCapacity: Int,
    body: (inout [Base.Iterator.Element]) throws->R
  ) rethrows -> R {
    var result: [Base.Iterator.Element]
    switch self {
      
    case .small(let base):
      if minCapacity <= numericCast(base.count) {
        result = Array(base)
      }
      else {
        result = []
        result.reserveCapacity(Swift.min(minCapacity, numericCast(base.count)))
        result.append(contentsOf: base)
      }
      
    case .large(let base):
      result = base
      result.reserveCapacity(minCapacity)
      self = .large([]) // Swap out result so we don't incur multiple refs
    }
    defer { self = .large(result) }
    return try body(&result)
  }

  /// Returns the small (`Base`) representation iff we have it and it has at
  /// least the given capacity.
  internal func _small(minCapacity: Int) -> Base? {
    if case .small(let base) = self,
      minCapacity <= numericCast(base.maxCapacity) {
      return base
    }
    return nil
  }
}

/// Initializers from underlying representations
extension UnboundCapacity {
  public init(_ b: Base) { self = .small(b) }
  public init(_ b: [Base.Iterator.Element]) { self = .large(b) }
}

extension UnboundCapacity : RangeReplaceableCollection {
  public init() {
    self.init(Base())
  }

  public mutating func replaceSubrange<C>(
    _ target: Range<Index>,
    with newElements: C
  ) where C : Collection, C.Iterator.Element == Iterator.Element {
    let newCount = count + numericCast(newElements.count) - target.count

    let r = _small(minCapacity: newCount)
    if _fastPath(r != nil), var s = r {
      s.replaceSubrange(
        s.index(atOffset: target.lowerBound)
        ..< s.index(atOffset: target.upperBound),
        with: newElements)
      self = .small(s)
      return
    }
    
    withMutableArray(minCapacity: newCount) {
      $0.replaceSubrange(target, with: newElements)
    }
  }

  public mutating func reserveCapacity(_ n: IndexDistance) {
    if _fastPath(_small(minCapacity: n) != nil) { return }
    withMutableArray(minCapacity: n) { _ in }
  }
}

