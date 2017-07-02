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
@_versioned
struct _SwiftUTF16StringHeader : _BoundedBufferHeader {
  var count: UInt32
  var capacity: UInt32
  var flags: UInt16
  
  public init(count: Int, capacity: Int) {
    self.count = numericCast(count)
    self.capacity = numericCast(capacity)
    self.flags = 0
  }
}

@_versioned
struct _SwiftLatin1StringHeader : _BoundedBufferHeader {
  var count: UInt32
  var capacity: UInt32
  var flags: UInt8
  
  public init(count: Int, capacity: Int) {
    self.count = numericCast(count)
    self.capacity = numericCast(capacity)
    self.flags = 0
  }
}

/// Common base class of our string storage classes
extension String {
  internal class _StorageBase<
    Header: _BoundedBufferHeader,
    Element: UnsignedInteger
  > :
  // Dynamically provides inheritance from NSString
  _SwiftNativeNSString {
    @nonobjc
    final internal var _header: Header

    // satisfies the compiler's demand for a designated initializer
    @nonobjc
    internal init(_doNotCallMe: ()) { fatalError("do not call me") }
    
    @objc
    final public func length() -> Int {
      return numericCast(_header.count)
    }

    @objc(copyWithZone:)
    final public func copy(with: _SwiftNSZone?) -> AnyObject {
      return self
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
}

extension String._StorageBase {
  @inline(__always)
  public static func _rawStorage(minCapacity n: Int) -> Self {
    return Builtin.allocWithTailElems_1(self, n._builtinWordValue, Element.self)
  }
}

//===--- UTF16 String Storage ---------------------------------------------===//
extension String {
  @_versioned
  internal final class _UTF16Storage
    : String._StorageBase<_SwiftUTF16StringHeader, UTF16.CodeUnit>,
      _NSStringCore {
    // WORKAROUND: helping type inference along will be unnecessary someday
    public typealias Element = UInt16
    public typealias Iterator = IndexingIterator<String._UTF16Storage>
    
    //===--- _NSStringCore conformance --------------------------------------===//
    // There doesn't seem to be a way to write these in an extension

    /// Returns a pointer to contiguously-stored UTF-16 code units
    /// comprising the whole string, or NULL if such storage isn't
    /// available.
    ///
    /// WARNING: don't use this method from Swift code; ARC may end the
    /// lifetime of self before you get a chance to use the result.
    @objc
    public func _fastCharacterContents() -> UnsafePointer<UInt16>? {
      return UnsafePointer(_baseAddress)
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
}


extension String._UTF16Storage : _BoundedBufferReference {
  @nonobjc
  public static var extraCapacity: Int { return 1 }
}

extension String._UTF16Storage /*: UnicodeStorage*/ {
  public typealias Encoding = Unicode.UTF16

  internal func _getFlags(_ r: CountableClosedRange<UInt8>) -> UInt16 {
    return _header.flags &>> r.lowerBound
      & (~0 as UInt16 &>> (16 - r.count))
  }
  
  internal func _setFlags(
    _ r: CountableClosedRange<UInt8>, to newValue: UInt16
  ) {
    _header.flags &= ~(
      (~0 as UInt16 &>> (16 - r.upperBound))
      ^ (~0 as UInt16 &>> (16 - r.lowerBound)))
    _header.flags |= newValue &<< r.lowerBound
  }
  
  internal func _getTriBool(_ i: UInt8) -> Bool? {
    let x = _getFlags(i...i+1)
    guard x != 0 else { return nil }
    return (x & 0x1) != 0
  }
  
  internal func _setTriBool(_ i: UInt8, to newValue: Bool?) {
    _setFlags(i...i+1, to: newValue == false ? 2 : newValue == true ? 3 : 0)
  }
  
  @nonobjc
  public var isASCII: Bool? {
    get { return _getTriBool(0) }
    set { _setTriBool(0, to: newValue) }
  }

  @nonobjc
  public var isLatin1: Bool? {
    get { return _getTriBool(2) }
    set { _setTriBool(2, to: newValue) }
  }

  @nonobjc
  public var isValidEncoding: Bool? {
    get { return _getTriBool(4) }
    set { _setTriBool(4, to: newValue) }
  }
  
  @nonobjc
  public var isFCCNormalized: Bool? {
    get { return _getTriBool(6) }
    set { _setTriBool(6, to: newValue) }
  }
  
  @nonobjc
  @_versioned
  internal func _setMaxStored(_ maxCodeUnit: UInt16) {
    isASCII = maxCodeUnit < 0x80
    isLatin1 = maxCodeUnit < 0x100
    if maxCodeUnit < 0x300 { isFCCNormalized = true }
    if maxCodeUnit < 0xD800 { isValidEncoding = true }
  }
  
  @inline(__always)
  public static func copying<Source : Collection>(
    _ source: Source,
    minCapacity: Int = 0,
    maxElement: UInt16? = nil
  ) -> String._UTF16Storage
  where Source.Iterator.Element == UInt16 {
    let r = instanceWithUnintializedElements(
      count: numericCast(source.count), minCapacity: minCapacity)
    defer { _fixLifetime(r) }
    var buf = r.withUnsafeMutableBufferPointer { $0 }
    source._copyCompleteContents(initializing: buf)
    r._setMaxStored(maxElement ?? source.max() ?? 0)
    return r
  }
}

//===--- Latin-1 String Storage -------------------------------------------===//
extension String {
  @_versioned
  internal final class _Latin1Storage
  : String._StorageBase<_SwiftLatin1StringHeader, UInt8>,
    _NSStringCore // Ensures that we implement essential NSString methods.  
  {
    // WORKAROUND: helping type inference along will be unnecessary someday
    public typealias Element = UInt8
    public typealias Iterator = IndexingIterator<String._Latin1Storage>
    
    //===--- _NSStringCore conformance --------------------------------------===//
    // There doesn't seem to be a way to write these in an extension

    /// Returns a pointer to contiguously-stored UTF-16 code units
    /// comprising the whole string, or NULL if such storage isn't
    /// available.
    ///
    /// WARNING: don't use this method from Swift code; ARC may end the
    /// lifetime of self before you get a chance to use the result.
    @objc
    public func _fastCharacterContents() -> UnsafePointer<UInt16>? {
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
    public override var _baseAddress: UnsafeMutablePointer<UInt8> {
      return UnsafeMutablePointer(
        Builtin.projectTailElems(self, Element.self))
    }
  }
}

extension String._Latin1Storage : _BoundedBufferReference {
  @nonobjc
  public static var extraCapacity: Int { return 1 }
}

extension String._Latin1Storage {
  internal func _getFlags(_ r: CountableClosedRange<UInt8>) -> UInt8 {
    return _header.flags &>> r.lowerBound
      & (~0 as UInt8 &>> (8 - r.count))
  }
  
  internal func _setFlags(
    _ r: CountableClosedRange<UInt8>, to newValue: UInt8
  ) {
    _header.flags &= ~(
      (~0 as UInt8 &>> (8 - r.upperBound))
      ^ (~0 as UInt8 &>> (8 - r.lowerBound)))
    _header.flags |= newValue &<< r.lowerBound
  }
  
  internal func _getTriBool(_ i: UInt8) -> Bool? {
    let x = _getFlags(i...i+1)
    guard x != 0 else { return nil }
    return (x & 0x1) != 0
  }
  
  internal func _setTriBool(_ i: UInt8, to newValue: Bool?) {
    _setFlags(i...i+1, to: newValue == false ? 2 : newValue == true ? 3 : 0)
  }

  @nonobjc
  public var isASCII: Bool? {
    get { return _getTriBool(0) }
    set { _setTriBool(0, to: newValue) }
  }

  public static func copying<Source : Collection>(
    _ source: Source,
    minCapacity: Int = 0,
    isASCII: Bool? = nil
  ) -> String._Latin1Storage
  where Source.Iterator.Element == UInt8 {
    let r = self.copying(source, minCapacity: minCapacity, _force: ())
    r.isASCII = isASCII ?? (source.max() ?? 0 < 0x80)
    return r
  }
}

//===--- Multi-Format String Storage --------------------------------------===//

@inline(__always)
public func _mkLatin1<C: Collection>(
  _ x: C, minCapacity: Int = 0, isASCII: Bool? = nil
) -> AnyObject
where C.Element == UInt8
{
  return String._Latin1Storage.copying(
    x, minCapacity: minCapacity, isASCII: isASCII)
}

@inline(__always)
public func _mkUTF16<C: Collection>(
  _ x: C, minCapacity: Int = 0, maxElement: UInt16? = nil
) -> AnyObject
where C.Element == UInt16
{
  return String._UTF16Storage.copying(
    x, minCapacity: minCapacity, maxElement: maxElement)
}
