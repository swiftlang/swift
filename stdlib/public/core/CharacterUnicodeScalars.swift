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
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct UnicodeScalarView {
    @usableFromInline // FIXME(sil-serialize-all)
    internal let _base: Character

    @inlinable // FIXME(sil-serialize-all)
    internal init(_base: Character) {
      self._base = _base
    }
  }
  
  @inlinable // FIXME(sil-serialize-all)
  public var unicodeScalars : UnicodeScalarView {
    return UnicodeScalarView(_base: self)
  }
}

extension Character.UnicodeScalarView {
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Iterator {
    @usableFromInline // FIXME(sil-serialize-all)
    internal var _base: IndexingIterator<Character.UnicodeScalarView>

    @inlinable // FIXME(sil-serialize-all)
    internal init(_base: IndexingIterator<Character.UnicodeScalarView>) {
      self._base = _base
    }
  }
}
    
extension Character.UnicodeScalarView.Iterator : IteratorProtocol {
  @inlinable // FIXME(sil-serialize-all)
  public mutating func next() -> UnicodeScalar? {
    return _base.next()
  }
}

extension Character.UnicodeScalarView : Sequence {
  @inlinable // FIXME(sil-serialize-all)
  public __consuming func makeIterator() -> Iterator {
    return Iterator(_base: IndexingIterator(_elements: self))
  }
}

extension Character.UnicodeScalarView {
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Index {
    @usableFromInline // FIXME(sil-serialize-all)
    internal let _encodedOffset: Int
    @usableFromInline // FIXME(sil-serialize-all)
    internal let _scalar: Unicode.UTF16.EncodedScalar
    @usableFromInline // FIXME(sil-serialize-all)
    internal let _stride: UInt8

    @inlinable // FIXME(sil-serialize-all)
    internal init(
      _encodedOffset: Int,
      _scalar: Unicode.UTF16.EncodedScalar, _stride: UInt8
    ) {
      self._encodedOffset = _encodedOffset
      self._scalar = _scalar
      self._stride = _stride
    }
  }
}

extension Character.UnicodeScalarView.Index : Equatable {
  @inlinable // FIXME(sil-serialize-all)
  public static func == (
    lhs: Character.UnicodeScalarView.Index,
    rhs: Character.UnicodeScalarView.Index
  ) -> Bool {
    return lhs._encodedOffset == rhs._encodedOffset
  }
}

extension Character.UnicodeScalarView.Index : Comparable {
  @inlinable // FIXME(sil-serialize-all)
  public static func < (
    lhs: Character.UnicodeScalarView.Index,
    rhs: Character.UnicodeScalarView.Index
  ) -> Bool {
    return lhs._encodedOffset < rhs._encodedOffset
  }
}

extension Character.UnicodeScalarView : Collection {
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Index {
    return index(
      after: Index(
        _encodedOffset: 0,
        _scalar: Unicode.UTF16.EncodedScalar(),
        _stride: 0
      ))
  }
  
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Index {
    return Index(
        _encodedOffset: _base._count,
        _scalar: Unicode.UTF16.EncodedScalar(),
        _stride: 0
      )
  }
  
  @inlinable // FIXME(sil-serialize-all)
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
      let c = _base._largeUTF16!.unmanagedView
      var i = c[c.index(c.startIndex, offsetBy: startOfNextScalar)...]
        .makeIterator()
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
  
  @inlinable // FIXME(sil-serialize-all)
  public subscript(_ i: Index) -> UnicodeScalar {
    return Unicode.UTF16.decode(i._scalar)
  }
}

extension Character.UnicodeScalarView : BidirectionalCollection {
  @inlinable // FIXME(sil-serialize-all)
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
      let c = _base._largeUTF16!.unmanagedView
      var i = c[..<c.index(c.startIndex, offsetBy: i._encodedOffset)]
        .reversed().makeIterator()
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

