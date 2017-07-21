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
    internal let _base: Character
  }
  
  public var unicodeScalars : UnicodeScalarView {
    return UnicodeScalarView(_base: self)
  }
}

extension Character.UnicodeScalarView {
  public struct Iterator {
    internal var _base: IndexingIterator<Character.UnicodeScalarView>
  }
}
    
extension Character.UnicodeScalarView.Iterator : IteratorProtocol {
  public mutating func next() -> UnicodeScalar? {
    return _base.next()
  }
}

extension Character.UnicodeScalarView : Sequence {
  public func makeIterator() -> Iterator {
    return Iterator(_base: IndexingIterator(_elements: self))
  }
}

extension Character.UnicodeScalarView {
  public struct Index {
    internal let _encodedOffset: Int
    internal let _scalar: Unicode.UTF16.EncodedScalar
    internal let _stride: UInt8
  }
}

extension Character.UnicodeScalarView.Index : Equatable {
  public static func == (
    lhs: Character.UnicodeScalarView.Index,
    rhs: Character.UnicodeScalarView.Index
  ) -> Bool {
    return lhs._encodedOffset == rhs._encodedOffset
  }
}

extension Character.UnicodeScalarView.Index : Comparable {
  public static func < (
    lhs: Character.UnicodeScalarView.Index,
    rhs: Character.UnicodeScalarView.Index
  ) -> Bool {
    return lhs._encodedOffset < rhs._encodedOffset
  }
}

extension Character.UnicodeScalarView : Collection {
  public var startIndex: Index {
    return index(
      after: Index(
        _encodedOffset: 0,
        _scalar: Unicode.UTF16.EncodedScalar(),
        _stride: 0
      ))
  }
  
  public var endIndex: Index {
    return Index(
        _encodedOffset: _base._smallUTF16?.count ?? _base._largeUTF16!.count,
        _scalar: Unicode.UTF16.EncodedScalar(),
        _stride: 0
      )
  }
  
  public func index(after i: Index) -> Index {
    var parser = Unicode.UTF16.ForwardParser()
    let startOfNextScalar = i._encodedOffset + numericCast(i._stride)
    let r: Unicode.ParseResult<Unicode.UTF16.EncodedScalar>
    
    let small_ = _base._smallUTF16
    if _fastPath(small_ != nil), let u16 = small_ {
      var i = u16[u16.index(u16.startIndex, offsetBy: startOfNextScalar)...]
        .makeIterator()
      r = parser.parseScalar(from: &i)
    }
    else {
      var i = _base._largeUTF16![startOfNextScalar...].makeIterator()
      r = parser.parseScalar(from: &i)
    }
    
    switch r {
    case .valid(let s):
      return Index(
        _encodedOffset: startOfNextScalar, _scalar: s,
        _stride: UInt8(truncatingIfNeeded: s.count))
    case .error:
      return Index(
        _encodedOffset: startOfNextScalar,
        _scalar: Unicode.UTF16.encodedReplacementCharacter,
        _stride: 1)
    case .emptyInput:
      if i._stride != 0 { return endIndex }
      fatalError("no position after end of Character's last Unicode.Scalar")
    }
  }
  
  public subscript(_ i: Index) -> UnicodeScalar {
    return Unicode.UTF16.decode(i._scalar)
  }
}

extension Character.UnicodeScalarView : BidirectionalCollection {
  public func index(before i: Index) -> Index {
    var parser = Unicode.UTF16.ReverseParser()
    let r: Unicode.ParseResult<Unicode.UTF16.EncodedScalar>
    
    let small_ = _base._smallUTF16
    if _fastPath(small_ != nil), let u16 = small_ {
      var i = u16[..<u16.index(u16.startIndex, offsetBy: i._encodedOffset)]
        .reversed().makeIterator()
      r = parser.parseScalar(from: &i)
    }
    else {
      var i = _base._largeUTF16![..<i._encodedOffset].reversed().makeIterator()
      r = parser.parseScalar(from: &i)
    }
    
    switch r {
    case .valid(let s):
      return Index(
        _encodedOffset: i._encodedOffset - s.count, _scalar: s,
        _stride: UInt8(truncatingIfNeeded: s.count))
    case .error:
      return Index(
        _encodedOffset: i._encodedOffset - 1,
        _scalar: Unicode.UTF16.encodedReplacementCharacter,
        _stride: 1)
    case .emptyInput:
      fatalError("no position before Character's last Unicode.Scalar")
    }
  }
}

