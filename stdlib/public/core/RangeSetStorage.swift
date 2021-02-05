//===----------------------------------------------------------*- swift -*-===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

/// Storage for a `RangeSet<T>`.
///
/// This is optimized to avoid allocating array storage for the common case of a
/// single range. The single range case is only attainable at initialization;
/// once added
internal struct _RangeSetStorage<T: Comparable> {
  private enum _Storage {
    case empty
    case singleRange(Range<T>)
    case variadic([Range<T>])
  }
  
  private var _storage: _Storage
  
  internal init() {
    _storage = .empty
  }
  
  internal init(_ range: Range<T>) {
    _storage = .singleRange(range)
  }
  
  internal init(_ ranges: [Range<T>]) {
    _storage = .variadic(ranges)
  }
}

// _RangeSetStorage has custom Equatable (and therefore Hashable)
// conformance, since the same "value" can be represented by different
// storage structures. For example, `.empty` and `.variadic([])` are
// equivalent, but the synthesized conformance treats them as distinct.
// The same holds with the `singleRange` representation and `variadic`
// with a single-element array.

extension _RangeSetStorage: Equatable {
  internal static func == (lhs: _RangeSetStorage, rhs: _RangeSetStorage) -> Bool {
    switch (lhs._storage, rhs._storage) {
    case (.empty, .empty):
      return true
    case (.empty, .singleRange), (.singleRange, .empty):
      return false
    case let (.empty, .variadic(ranges)),
         let (.variadic(ranges), .empty):
      return ranges.isEmpty
      
    case let (.singleRange(lhs), .singleRange(rhs)):
      return lhs == rhs
      
    case let (.singleRange(singleRange), .variadic(ranges)),
         let (.variadic(ranges), .singleRange(singleRange)):
      return ranges.count == 1 &&
        (ranges[0]) == singleRange
      
    case let (.variadic(lhsRanges), .variadic(rhsRanges)):
      return lhsRanges == rhsRanges
    }
  }
}

extension _RangeSetStorage: Hashable where T: Hashable {
  internal func hash(into hasher: inout Hasher) {
    for range in self {
      hasher.combine(range)
    }
  }
}

extension _RangeSetStorage: RandomAccessCollection, MutableCollection {
  internal var startIndex: Int { 0 }
  
  internal var endIndex: Int {
    switch _storage {
    case .empty: return 0
    case .singleRange: return 1
    case let .variadic(ranges): return ranges.count
    }
  }
  
  internal subscript(i: Int) -> Range<T> {
    get {
      switch _storage {
      case .empty:
        _preconditionFailure("Can't access elements of empty storage")
      case let .singleRange(range):
        _precondition(i == 0)
        return range
      case let .variadic(ranges):
        return ranges[i]
      }
    }
    set {
      switch _storage {
      case .empty:
        _preconditionFailure("Can't access elements of empty storage")
      case .singleRange:
        _precondition(i == 0)
        _storage = .singleRange(newValue)
      case .variadic(var ranges):
        // Temporarily set `_storage` to empty so that `ranges`
        // remains uniquely referenced while mutating.
        _storage = .empty
        ranges[i] = newValue
        _storage = .variadic(ranges)
      }
    }
  }
  
  internal var count: Int {
    switch _storage {
    case .empty: return 0
    case .singleRange: return 1
    case let .variadic(ranges): return ranges.count
    }
  }
}

extension _RangeSetStorage: RangeReplaceableCollection {
  internal mutating func replaceSubrange<C>(
    _ subrange: Range<Int>, with newElements: C
  ) where C : Collection, C.Element == Element {
    switch _storage {
    case .empty:
      if !newElements.isEmpty {
        _storage = .variadic(Array(newElements))
      }
      
    case let .singleRange(singleRange):
      switch (subrange.isEmpty, newElements.isEmpty) {
      case (false, true):
        // Replacing the single range with an empty collection.
        _storage = .empty
        
      case (false, false):
        // Replacing the single range with a non-empty collection;
        // promote to a variadic container.
        _storage = .variadic(Array(newElements))
        
      case (true, true):
        // Inserting an empty collection; no-op.
        break
        
      case (true, false):
        // Inserting a non-empty collection either before or after
        // the existing single element.
        var ranges: [Range<T>]
        if subrange.lowerBound == 0 {
          ranges = Array(newElements)
          ranges.append(singleRange)
        } else {
          ranges = [singleRange]
          ranges.append(contentsOf: newElements)
        }
        _storage = .variadic(ranges)
      }
      
    case .variadic(var ranges):
      // Temporarily set `_storage` to empty so that `ranges`
      // remains uniquely referenced while mutating.
      _storage = .empty
      ranges.replaceSubrange(subrange, with: newElements)
      _storage = .variadic(ranges)
    }
  }
}

