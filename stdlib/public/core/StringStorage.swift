//===--- StringStorage.swift ----------------------------------------------===//
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
import SwiftShims
extension _SwiftUTF16StringHeader : _BoundedBufferHeader {
  public init(count: Int, capacity: Int) {
    self.count = numericCast(count)
    self.capacity = numericCast(capacity)
    self.flags = 0
  }
}
extension _SwiftLatin1StringHeader : _BoundedBufferHeader {
  public init(count: Int, capacity: Int) {
    self.count = numericCast(count)
    self.capacity = numericCast(capacity)
    self.flags = 0
  }
}

/// Common base class of our string storage classes
public class _StringStorageBase<
  Header: _BoundedBufferHeader,
  Element: UnsignedInteger
> :
  // Dynamically provides inheritance from NSString
  _SwiftNativeNSString,
  // Allows us to code init in terms of Builtin.allocWithTailElems_1
  FactoryInitializable  
{
  final public var _header: _SwiftUTF16StringHeader
  
  @objc
  final public func length() -> Int {
    return numericCast(_header.count)
  }

  @objc(copyWithZone:)
  final public func copy(with: _SwiftNSZone?) -> AnyObject {
    return self
  }
  
  // satisfies the compiler's demand for a designated initializer
  @nonobjc
  internal init(_doNotCallMe: ()) { fatalError("do not call me") }

  @nonobjc
  public convenience init(uninitializedWithMinimumCapacity n: Int) {
    self.init(
      Builtin.allocWithTailElems_1(
        type(of: self), n._builtinWordValue, Element.self))
  }

  @objc
  final public func characterAtIndex(_ index: Int) -> UInt16 {
    defer { _fixLifetime(self) }
    return numericCast(_baseAddress[index])
  }

  @nonobjc
  var _baseAddress: UnsafeMutablePointer<Element> {
    // WORKAROUND: rdar://31047127 prevents us from implementing _baseAddress as
    // final here.
    fatalError("Override me!")
  }
}

//===--- UTF16 String Storage ---------------------------------------------===//
public final class _UTF16StringStorage
  : _StringStorageBase<_SwiftUTF16StringHeader, UTF16.CodeUnit>
  , _NSStringCore // Ensures that we implement essential NSString methods.  
{
  // WORKAROUND: helping type inference along will be unnecessary someday
  public typealias _Element = UInt16
  public typealias Iterator = IndexingIterator<_UTF16StringStorage>
  
  //===--- _NSStringCore conformance --------------------------------------===//
  // There doesn't seem to be a way to write these in an extension

  /// Returns a pointer to contiguously-stored UTF-16 code units
  /// comprising the whole string, or NULL if such storage isn't
  /// available.
  ///
  /// WARNING: don't use this method from Swift code; ARC may end the
  /// lifetime of self before you get a chance to use the result.
  @objc
  public func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    return _baseAddress
  }

  /// Returns a pointer to contiguously-stored code units in the
  /// system encoding comprising the whole string, or NULL if such
  /// storage isn't available.
  ///
  // WARNING: don't use this method from Swift code; ARC may end the lifetime of
  // self before you get a chance to use the result.
  // WARNING: Before you implement this as anything other than “return nil,”
  // see https://github.com/apple/swift/pull/3151#issuecomment-285583557
  @objc
  public func _fastCStringContents(
    _ nullTerminationRequired: Int8
  ) -> UnsafePointer<CChar>? {
    return nil
  }

  // WORKAROUND: rdar://31047127 prevents us from hoisting this into
  // _StringStorageBase
  @nonobjc
  public override var _baseAddress: UnsafeMutablePointer<UTF16.CodeUnit> {
    return UnsafeMutablePointer(
      Builtin.projectTailElems(self, Element.self))
  }
}

extension _UTF16StringStorage : _BoundedBufferReference {
  /// Returns empty singleton that is used for every single empty String.
  /// The contents of the storage should never be mutated.
  @nonobjc
  public static func _emptyInstance() -> _UTF16StringStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyStringStorage))
  }
}

extension _UTF16StringStorage : _FixedFormatUnicode {
  public typealias Encoding = UTF16

  // WORKAROUND: helping type inference along will be unnecessary someday
  public typealias UTF16View = _UTF16StringStorage
  public typealias CodeUnits = _UTF16StringStorage
  public typealias FCCNormalizedUTF16View = _UnicodeViews<
    CodeUnits, Encoding
  >.FCCNormalizedUTF16View
  
  public typealias CharacterView = _UnicodeViews<CodeUnits,Encoding>.CharacterView
  public typealias UnicodeScalarView = _UnicodeViews<CodeUnits,Encoding>.Scalars
  
  public var encoding: UTF16.Type { return UTF16.self }
  public var codeUnits: _UTF16StringStorage { return self }
  
  @nonobjc
  public var isKnownASCII: Bool {
    get { return _header.flags & 1<<0 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<0 as UInt16 }
      else { _header.flags &= ~(1<<0) }
    }
  }

  @nonobjc
  public var isKnownLatin1: Bool {
    get { return _header.flags & 1<<1 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<1 as UInt16 }
      else { _header.flags &= ~(1<<1) as UInt16 }
    }
  }
  
  @nonobjc
  public var isKnownValidEncoding: Bool {
    get { return _header.flags & 1<<2 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<2 as UInt16 }
      else { _header.flags &= ~(1<<2) as UInt16 }
    }
  }
  
  @nonobjc
  public var isKnownFCCNormalized: Bool {
    get { return _header.flags & 1<<3 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<3 as UInt16 }
      else { _header.flags &= ~(1<<3) as UInt16 }
    }
  }
  
  @nonobjc
  public var isKnownNFCNormalized: Bool {
    get { return _header.flags & 1<<4 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<4 as UInt16 }
      else { _header.flags &= ~(1<<4) as UInt16 }
    }
  }
  
  @nonobjc
  public var isKnownNFDNormalized: Bool {
    get { return _header.flags & 1<<5 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<5 as UInt16 }
      else { _header.flags &= ~(1<<5) as UInt16 }
    }
  }

  
  @nonobjc
  internal func _setMaxStored(_ maxCodeUnit: UInt16) {
    switch maxCodeUnit {
    case 0..<0x80: self.isKnownASCII = true; fallthrough
    case 0..<0x100: self.isKnownLatin1 = true; fallthrough
    case 0..<0x300: self.isKnownFCCNormalized = true; fallthrough
    case 0..<0xD800: self.isKnownValidEncoding = true
    default: break
    }
  }
  
  /// Initialize from a sequence of valid UTF16 code unit values (possibly
  /// represented with a different code unit type).
  @nonobjc
  public convenience init<OtherCodeUnits : Collection>(
    utf16CodeUnitValues: OtherCodeUnits,
    isKnownASCII: Bool = false
  )
  // FIXME: when new integers land, we won't need this constraint anymore.
  where OtherCodeUnits.Iterator.Element : UnsignedInteger {
    // No need for transcoding, since we're not trying to ensure our own
    // encoding is valid UTF16.  We'll just copy the same code units (possibly
    // zero-extended).
    self.init(count: numericCast(utf16CodeUnitValues.count))
    
    var maxCodeUnit: UInt16 = 0
    withUnsafeMutableBufferPointer {
      if _fastPath(isKnownASCII || utf16CodeUnitValues.isEmpty) {
        // Don't look for the maximal code unit value; we already know everything
        // we can learn from it
        utf16CodeUnitValues.lazy.map {
          numericCast($0)
        }._copyCompleteContents(initializing: $0)
      }
      else {
        // FIXME: hoping for loop fusion here; check to make sure we get it.
        maxCodeUnit = numericCast(utf16CodeUnitValues.max()!)
        utf16CodeUnitValues.lazy.map {
          numericCast($0)
        }._copyCompleteContents(initializing: $0)
      }
    }
    _setMaxStored(maxCodeUnit)
  }

  @nonobjc
  public convenience init(
    count: Int,
    minimumCapacity: Int = 0
  ) {
    self.init(minimumCapacity: Swift.max(count, minimumCapacity)) {
      _SwiftUTF16StringHeader(
        count: UInt32(count), capacity: UInt32($0), flags: 0)
    }
  }

  // Eventually this should work for _AnyUnicode existentials too.
  @nonobjc
  public convenience init<Other: _FixedFormatUnicode>(
    _ other: Other
  )
  where Other.CodeUnits.Iterator.Element : UnsignedInteger,

  // FIXME: drop these constraints once we have the compiler features.
  Other.CodeUnits.Index == Other.CodeUnits.SubSequence.Index, 
  Other.CodeUnits.SubSequence : RandomAccessCollection, 
  Other.CodeUnits.SubSequence == Other.CodeUnits.SubSequence.SubSequence, 
  Other.CodeUnits.Iterator.Element == Other.CodeUnits.SubSequence.Iterator.Element, 
  Other.CodeUnits.SubSequence.Iterator.Element == Other.Encoding.EncodedScalar.Iterator.Element  
  {
    let otherIsLatin1 = Other.Encoding.self is Latin1.Type
    let otherIsUTF16Compatible = otherIsLatin1
      || Other.Encoding.EncodedScalar.self is UTF16.EncodedScalar.Type

    if _fastPath(otherIsUTF16Compatible) {
      self.init(
        utf16CodeUnitValues: other.codeUnits,
        isKnownASCII: other.isKnownASCII)
      
      if other.isKnownValidEncoding { isKnownValidEncoding = true }
      if other.isKnownFCCNormalized { isKnownFCCNormalized = true }
      if other.isKnownNFCNormalized { isKnownNFCNormalized = true }
      if other.isKnownNFDNormalized { isKnownNFDNormalized = true }
    }
    else {
      var count = 0
      var maxCodeUnit: UInt16 = 0
      Other.Encoding.parseForward(
        other.codeUnits,
        repairingIllFormedSequences: true
      ) {
        count += $0.utf16.count
        maxCodeUnit = Swift.max(maxCodeUnit, $0.utf16.max()!)
      }

      self.init(count: count)
      withUnsafeMutableBufferPointer {
        _UnicodeViews(
          other.codeUnits, Other.Encoding.self).transcoded(
          to: UTF16.self
        )._copyCompleteContents(initializing: $0)
      }
      _setMaxStored(maxCodeUnit)
      isKnownValidEncoding = true // repairs had to be made
    }
  }
}

//===--- Latin-1 String Storage -------------------------------------------===//
public final class _Latin1StringStorage
  : _StringStorageBase<_SwiftLatin1StringHeader, UInt8>
  , _NSStringCore // Ensures that we implement essential NSString methods.  
{
  // WORKAROUND: helping type inference along will be unnecessary someday
  public typealias _Element = UInt8
  public typealias Iterator = IndexingIterator<_Latin1StringStorage>
  
  /// Returns a pointer to contiguously-stored UTF-16 code units
  /// comprising the whole string, or NULL if such storage isn't
  /// available.
  ///
  /// WARNING: don't use this method from Swift code; ARC may end the
  /// lifetime of self before you get a chance to use the result.
  @objc
  public func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    return nil
  }

  /// Returns a pointer to contiguously-stored code units in the
  /// system encoding comprising the whole string, or NULL if such
  /// storage isn't available.
  ///
  // WARNING: don't use this method from Swift code; ARC may end the lifetime of
  // self before you get a chance to use the result.
  // WARNING: Before you implement this as anything other than “return nil,”
  // see https://github.com/apple/swift/pull/3151#issuecomment-285583557
  @objc
  public func _fastCStringContents(
    _ nullTerminationRequired: Int8
  ) -> UnsafePointer<CChar>? {
    return nil
  }
  
  // WORKAROUND: rdar://31047127 prevents us from hoisting this into
  // _StringStorageBase
  @nonobjc
  public override var _baseAddress: UnsafeMutablePointer<Latin1.CodeUnit> {
    return UnsafeMutablePointer(
      Builtin.projectTailElems(self, Element.self))
  }
}

extension _Latin1StringStorage : _BoundedBufferReference {
  @nonobjc
  public static func _emptyInstance() -> _Latin1StringStorage {
    return _Latin1StringStorage(uninitializedWithMinimumCapacity: 0)
  }
}

extension _Latin1StringStorage : _FixedFormatUnicode {
  public typealias Encoding = Latin1
  
  // WORKAROUND: helping type inference along will be unnecessary someday
  public typealias CodeUnits = _Latin1StringStorage
  public typealias FCCNormalizedUTF16View = LazyMapRandomAccessCollection<CodeUnits, UTF16.CodeUnit>
  public typealias UTF16View = FCCNormalizedUTF16View
  
  public var codeUnits: CodeUnits { return self }

  public var isKnownNFDNormalized: Bool { return true }
  public var isKnownNFCNormalized: Bool { return true }

  @nonobjc
  public var isKnownASCII: Bool {
    get { return _header.flags & 1<<0 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<0 as UInt16 }
      else { _header.flags &= ~(1<<0) }
    }
  }
}

//===--- UTF-8 String Storage ---------------------------------------------===//
public struct UTF8StringHeader : _BoundedBufferHeader {
  public var count: UInt32
  public var capacity: UInt32
  
  public init(count: Int, capacity: Int) {
    self.count = numericCast(count)
    self.capacity = numericCast(capacity)
  }
}

public final class _UTF8StringStorage
// FIXME: we might want our own header type
  : _StringStorageBase<UTF8StringHeader, UInt8>
  , _NSStringCore // Ensures that we implement essential NSString methods.  
{
  // WORKAROUND: helping type inference along will be unnecessary someday
  public typealias _Element = UInt8
  public typealias Iterator = IndexingIterator<_UTF8StringStorage>
  
  /// Returns a pointer to contiguously-stored UTF-16 code units
  /// comprising the whole string, or NULL if such storage isn't
  /// available.
  ///
  /// WARNING: don't use this method from Swift code; ARC may end the
  /// lifetime of self before you get a chance to use the result.
  @objc
  public func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    return nil
  }

  /// Returns a pointer to contiguously-stored code units in the
  /// system encoding comprising the whole string, or NULL if such
  /// storage isn't available.
  ///
  // WARNING: don't use this method from Swift code; ARC may end the lifetime of
  // self before you get a chance to use the result.
  // WARNING: Before you implement this as anything other than “return nil,”
  // see https://github.com/apple/swift/pull/3151#issuecomment-285583557
  @objc
  public func _fastCStringContents(
    _ nullTerminationRequired: Int8
  ) -> UnsafePointer<CChar>? {
    return nil
  }
  
  // WORKAROUND: rdar://31047127 prevents us from hoisting this into
  // _StringStorageBase
  @nonobjc
  public override var _baseAddress: UnsafeMutablePointer<UTF8.CodeUnit> {
    return UnsafeMutablePointer(
      Builtin.projectTailElems(self, Element.self))
  }

  @nonobjc
  public var isKnownASCII = false
  @nonobjc
  public var isKnownLatin1 = false
  @nonobjc
  public var isKnownValidEncoding = false
  @nonobjc
  public var isKnownFCCNormalized = false
  @nonobjc
  public var isKnownFCDForm = false
  @nonobjc
  public var isKnownNFDNormalized = false
  @nonobjc
  public var isKnownNFCNormalized = false
}

extension _UTF8StringStorage : _BoundedBufferReference {
  @nonobjc
  public static func _emptyInstance() -> _UTF8StringStorage {
    return _UTF8StringStorage(uninitializedWithMinimumCapacity: 0)
  }
}

extension _UTF8StringStorage : _FixedFormatUnicode {
  public typealias Encoding = UTF8
  
  // WORKAROUND: helping type inference along will be unnecessary someday
  public typealias CodeUnits = _UTF8StringStorage
  
  public typealias UTF16View = _UnicodeViews<
    CodeUnits, Encoding
  >.TranscodedView<UTF16>

  public typealias FCCNormalizedUTF16View = _UnicodeViews<
    CodeUnits, Encoding
  >.FCCNormalizedUTF16View

  public typealias CharacterView = _UnicodeViews<CodeUnits,Encoding>.CharacterView
  public typealias UnicodeScalarView = _UnicodeViews<CodeUnits,Encoding>.Scalars
  
  public var codeUnits: CodeUnits { return self }
  public var utf16: UTF16View { return UTF16View(self) }
  public var fccNormalizedUTF16: FCCNormalizedUTF16View {
    return _UnicodeViews(self, UTF8.self).fccNormalizedUTF16
  }
}

