// Prototype code to be added to an eventual StringStorage.swift

import Swift
import SwiftShims

protocol CopyConstructible {  }
extension CopyConstructible {
  init(_ me: Self) {
    self = me
  }
}

/// Buffer of contiguously-stored UTF-16-compatible code units, meaning it can
/// hold ASCII, Latin1, or UTF-16.
/// 
/// - Requires: Element is trivial (UInt8/UInt16 in practice)
@_versioned
final class _StringStorage<Element: UnsignedInteger>
 : _SwiftNativeNSString, _NSStringCore, CopyConstructible {

  @nonobjc
  var _header: _SwiftStringBodyStorage
  
  @nonobjc
  var count: Int {
    get { return numericCast(_header.count) }
    set { _header.count = numericCast(newValue) }
  }
  
  @nonobjc
  var capacity: Int {
    get { return numericCast(_header.capacity) }
    set { _header.capacity = numericCast(newValue) }
  }

  @nonobjc
  var isKnownLatin1: Bool {
    get { return _header.flags & 1<<0 as UInt8 != 0 }
    set {
      if newValue { _header.flags |= 1<<0 as UInt8 }
      else { _header.flags &= ~(1<<0) as UInt8 }
    }
  }
  
  @nonobjc
  var isKnownASCII: Bool {
    get { return _header.flags & 1<<1 as UInt8 != 0 }
    set {
      if newValue { _header.flags |= 1<<1 as UInt8 }
      else { _header.flags &= ~(1<<1) }
    }
  }

  @nonobjc
  var isKnownValidEncoding: Bool {
    get { return _header.flags & 1<<2 as UInt8 != 0 }
    set {
      if newValue { _header.flags |= 1<<2 as UInt8 }
      else { _header.flags &= ~(1<<2) as UInt8 }
    }
  }
  
  @nonobjc
  var isKnownFCCNormalized: Bool {
    get { return _header.flags & 1<<3 as UInt8 != 0 }
    set {
      if newValue { _header.flags |= 1<<3 as UInt8 }
      else { _header.flags &= ~(1<<3) as UInt8 }
    }
  }

  /// Satisfies the compiler's need for a designated initializer.
  @nonobjc
  internal init(_NeverActuallyCalled: ()) {
    fatalError("Unexpected call to StringStorage designated initializer")
  }

  @nonobjc
  convenience init(
    count: Int,
    minimumCapacity: Int = 0,
    isKnownLatin1: Bool = false,
    isKnownASCII: Bool = false,
    isKnownValidEncoding: Bool = false,
    isKnownFCCNormalized: Bool = false
  ) {
    self.init(
      Builtin.allocWithTailElems_1(
      _StringStorage.self,
        Swift.max(count, minimumCapacity)._builtinWordValue, Element.self))

    let storageAddr = UnsafeRawPointer(
      Builtin.projectTailElems(self, Element.self))
    let startAddr = UnsafeRawPointer(Builtin.bridgeToRawPointer(self))
    let endAddr = startAddr + _swift_stdlib_malloc_size(startAddr)
    let realCapacity = endAddr.assumingMemoryBound(to: Element.self)
      - storageAddr.assumingMemoryBound(to: Element.self)

    // All stored properties are uninitialized, but we can use assignment
    // because they're trivial types.
    //
    // FIXME: we should abstract this pattern and make it use actual
    // initialization, modifying ManagedBufferPointer to do it.
    self._header = _SwiftStringBodyStorage(
      count: numericCast(count), capacity: numericCast(realCapacity), flags: 0)
  }

  /// The empty singleton that is used for every single empty String.
  /// The contents of the storage should never be mutated.
  @nonobjc
  internal static var empty: _StringStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyStringStorage))
  }

  @nonobjc
  func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>)->R
  ) -> R {
    defer { _fixLifetime(self) }
    let base = UnsafeMutablePointer<Element>(
      Builtin.projectTailElems(self, Element.self))
    var buffer = UnsafeMutableBufferPointer(start: base, count: count)
    return body(&buffer)
  }

  @nonobjc
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>)->R
  ) -> R {
    return withUnsafeMutableBufferPointer {
      body(UnsafeBufferPointer(start: $0.baseAddress, count: $0.count))
    }
  }
  /*
}
  https://bugs.swift.org/browse/SR-4173 @objc is not supported within extensions of generic classes
extension _StringStorage : _NSStringCore {
*/

  @objc
  func length() -> Int {
    return count
  }

  @objc
  func characterAtIndex(_ index: Int) -> UInt16 {
    return numericCast(self[index])
  }

  @objc
  func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    guard Element.self is UInt16.Type else { return nil }

    // Need to do manual projection, because compiler can't prove
    // firstElementAddress has the right type.
    return UnsafeMutablePointer(Builtin.projectTailElems(self, Element.self))
  }

  // WARNING: Before you implement this as anything other than “return nil,”
  // see https://github.com/apple/swift/pull/3151#issuecomment-285583557
  @objc
  public func _fastCStringContents(
    _ nullTerminationRequired: Int8
  ) -> UnsafePointer<CChar>? {
    return nil
  }
  
  @objc(copyWithZone:)
  internal func copy(with _: _SwiftNSZone?) -> AnyObject {
    return self
  }
}

extension _StringStorage : RandomAccessCollection, MutableCollection {
  @nonobjc
  var startIndex : Int { return 0 }
  @nonobjc
  var endIndex : Int { return count }

  @nonobjc
  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return _StringBuffer(self)[i]
    }
    set {
      _StringBuffer(self)[i] = newValue
    }
  }
}

extension Collection {
  func _copyCompleteContents(
    initializing memory: UnsafeMutableBufferPointer<Iterator.Element>
  ) {
    var (excessElements, endOfCopy) = self._copyContents(initializing: memory)
    _precondition(
      excessElements.next() == nil,
      "Source of new string under-reported its count")
    _precondition(
      endOfCopy == memory.endIndex,
      "Source of new string over-reported its count")
  }
}

extension _StringStorage where Element == UInt16 {
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
  internal convenience init<OtherCodeUnits, OtherEncoding>(
    utf16CodeUnitValues other: UnicodeStorage<OtherCodeUnits, OtherEncoding>,
    isKnownLatin1 otherIsKnownLatin1: Bool = false,
    isKnownASCII otherIsKnownASCII: Bool = false,
    isKnownValidEncoding otherIsKnownValidEncoding: Bool = false,
    isKnownFCCNormalized otherIsKnownFCCNormalized: Bool = false
  )
  // FIXME: when new integers land, we won't need this constraint anymore.
  where OtherCodeUnits.Iterator.Element : UnsignedInteger
  {
    let otherIsLatin1 = OtherEncoding.self is Latin1.Type
    
    // No need for transcoding, since we're not trying to ensure our own
    // encoding is valid UTF16.  We'll just copy the same code units (possibly
    // zero-extended).
    self.init(
      count: numericCast(other.codeUnits.count),
      minimumCapacity: 0,
      isKnownLatin1: otherIsKnownLatin1 || otherIsLatin1,
      isKnownASCII: otherIsKnownASCII,
      isKnownValidEncoding: otherIsKnownValidEncoding
      || otherIsLatin1 || OtherEncoding.self is ValidUTF16.Type,
      isKnownFCCNormalized: otherIsKnownFCCNormalized || otherIsLatin1
    )
    
    if _fastPath(otherIsKnownASCII) {
      withUnsafeMutableBufferPointer {
        other.codeUnits.lazy.map {
          numericCast($0)
        }._copyCompleteContents(initializing: $0)
      }
    }
    else {
      var maxCodeUnit: UInt16 = 0
      withUnsafeMutableBufferPointer {
        for (i, u) in other.codeUnits.enumerated() {
          let u16 = numericCast(u) as UInt16
          maxCodeUnit = Swift.max(u16, maxCodeUnit)
          _precondition(i < count)
          $0[i] = u16
        }
      }
      _setMaxStored(maxCodeUnit)
    }
  }

  
  @nonobjc
  internal convenience init<OtherCodeUnits, OtherEncoding>(
    _ other: UnicodeStorage<OtherCodeUnits, OtherEncoding>,
    isKnownLatin1 otherIsKnownLatin1: Bool = false,
    isKnownASCII otherIsKnownASCII: Bool = false,
    isKnownValidEncoding otherIsKnownValidEncoding: Bool = false,
    isKnownFCCNormalized otherIsKnownFCCNormalized: Bool = false,
    box: () = ()
  )
  // FIXME: when new integers land, we won't need this constraint anymore.
  where OtherCodeUnits.Iterator.Element : UnsignedInteger
  {
    let otherIsLatin1 = OtherEncoding.self is Latin1.Type
    let otherIsUTF16Compatible = otherIsLatin1
      || OtherEncoding.EncodedScalar.self is UTF16.EncodedScalar.Type

    if _fastPath(otherIsUTF16Compatible) {
      self.init(
        utf16CodeUnitValues: other,
        isKnownLatin1: otherIsKnownLatin1,
        isKnownASCII: otherIsKnownASCII,
        isKnownValidEncoding: otherIsKnownValidEncoding,
        isKnownFCCNormalized: otherIsKnownFCCNormalized
      )
    }
    else {

      var count = 0
      var maxCodeUnit: UInt16 = 0
      OtherEncoding.parseForward(
        other.codeUnits,
        repairingIllFormedSequences: true
      ) {
        count += $0.utf16.count
        maxCodeUnit = Swift.max(maxCodeUnit, $0.utf16.max()!)
      }

      self.init(count: count)
      withUnsafeMutableBufferPointer {
        other.transcoded(
          to: UTF16.self
        )._copyCompleteContents(initializing: $0)
      }
      _setMaxStored(maxCodeUnit)
    }
  }    
}

/// - Requires: Element is trivial (UInt8/UInt16)
struct _StringBuffer<Element: UnsignedInteger> {
  internal var _storage: _StringStorage<Element>

  init(_ storage: _StringStorage<Element>) { self._storage = storage }

  init() {
    self.init(_StringStorage.empty)
  }

  init(_buffer source: Buffer, shiftedToStartIndex: Int) {
    _sanityCheck(shiftedToStartIndex == 0, "shiftedToStartIndex must be 0")
    self.init(source._storage)
  }

  init(_uninitializedCount: Int, minimumCapacity: Int) {
    self.init(_StringStorage(count: _uninitializedCount, 
      minimumCapacity: minimumCapacity))
  }

}

extension _StringBuffer : _ArrayBufferProtocol, _ContiguousBufferProtocol {
  var count: Int { 
    get { return _storage.count }
    nonmutating set { _storage.count = newValue }
  }

  var capacity: Int { return _storage.capacity }
  var owner: AnyObject { return _storage }
  
  var firstElementAddress: UnsafeMutablePointer<Element> {
    return UnsafeMutablePointer(Builtin.projectTailElems(_storage, Element.self))
  }
  
  var firstElementAddressIfContiguous: UnsafeMutablePointer<Element>? {
    return firstElementAddress
  }
  
  var identity: UnsafeRawPointer { return UnsafeRawPointer(firstElementAddress) }

  internal mutating func isUniquelyReferenced() -> Bool {
    return _isUnique(&_storage)
  }

  internal mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
  -> _StringBuffer? {
    if _fastPath(isUniquelyReferenced()) {
      if _fastPath(capacity >= minimumCapacity) {
        return self
      }
    }
    return nil
  }

  mutating func isMutableAndUniquelyReferenced() -> Bool {
    return isUniquelyReferenced()
  }

  func requestNativeBuffer() -> _StringBuffer? {
    return self
  }

  @_versioned
  internal func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeMutableBufferPointer(
        start: firstElementAddress,
        count: count
      )
    )
  }

  @_versioned
  internal func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeBufferPointer(
        start: firstElementAddress,
        count: count
      )
    )
  }

  @discardableResult
  internal func _copyContents(
    subRange bounds: Range<Int>,
    initializing target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element> {
    _sanityCheck(bounds.lowerBound >= 0)
    _sanityCheck(bounds.upperBound >= bounds.lowerBound)
    _sanityCheck(bounds.upperBound <= count)

    defer { _fixLifetime(self) }

    let initializedCount = bounds.upperBound - bounds.lowerBound
    target.initialize(
      from: firstElementAddress + bounds.lowerBound, count: initializedCount)
    
    return target + initializedCount
  }

}

extension _StringBuffer : RandomAccessCollection, MutableCollection, 
    RangeReplaceableCollection {
  var startIndex : Int { return _storage.startIndex }
  var endIndex : Int { return _storage.endIndex }

  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    nonmutating set {
      withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
}
