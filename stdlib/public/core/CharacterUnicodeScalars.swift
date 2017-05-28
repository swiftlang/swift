//===--- CharacterUnicodeScalars.swift ------------------------------------===//
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
extension Character {
  public struct UnicodeScalarView {
    internal let _base: String.UnicodeScalarView
  }
  
  public var unicodeScalars : UnicodeScalarView {
    return UnicodeScalarView(_base: String(self).unicodeScalars)
  }
}

extension Character.UnicodeScalarView {
  public struct Iterator {
    internal var _base: String.UnicodeScalarView.Iterator
  }
}
    
extension Character.UnicodeScalarView.Iterator : IteratorProtocol {
  public mutating func next() -> UnicodeScalar? {
    return _base.next()
  }
}

extension Character.UnicodeScalarView : Sequence {
  public func makeIterator() -> Iterator {
    return Iterator(_base: _base.makeIterator())
  }
}

extension Character.UnicodeScalarView {
  public struct Index {
    internal let _base: String.UnicodeScalarView.Index
  }
}

extension Character.UnicodeScalarView.Index : Equatable {
  public static func == (
    lhs: Character.UnicodeScalarView.Index,
    rhs: Character.UnicodeScalarView.Index
  ) -> Bool {
    return lhs._base == rhs._base
  }
}

extension Character.UnicodeScalarView.Index : Comparable {
  public static func < (
    lhs: Character.UnicodeScalarView.Index,
    rhs: Character.UnicodeScalarView.Index
  ) -> Bool {
    return lhs._base < rhs._base
  }
}

extension Character.UnicodeScalarView : Collection {
  public var startIndex: Index {
    return Index(_base: _base.startIndex)
  }
  public var endIndex: Index {
    return Index(_base: _base.endIndex)
  }
  public func index(after i: Index) -> Index {
    return Index(_base: _base.index(after: i._base))
  }
  public subscript(_ i: Index) -> UnicodeScalar {
    return _base[i._base]
  }
}

extension Character.UnicodeScalarView : BidirectionalCollection {
  public func index(before i: Index) -> Index {
    return Index(_base: _base.index(before: i._base))
  }
}
