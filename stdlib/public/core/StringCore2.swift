//===--- StringCore2.swift ------------------------------------------------===//
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

extension _BoundedBufferReference {
  /// Calls `body` on a mutable buffer that covers the entire extent of
  /// allocated memory.
  func _withMutableCapacity<R>(
    body: (inout UnsafeMutableBufferPointer<Element>)->R
  ) -> R {
    return self.withUnsafeMutableBufferPointer { buf in
      var fullBuf = UnsafeMutableBufferPointer(
        start: buf.baseAddress, count: capacity)
      return body(&fullBuf)
    }
  }
}

internal struct _Concat3<C0: Collection, C1: Collection, C2: Collection>
where C0.Element == C1.Element, C1.Element == C2.Element {
  var c0: C0
  var c1: C1
  var c2: C2

  init(_ c0: C0, _ c1: C1, _ c2: C2) {
    self.c0 = c0
    self.c1 = c1
    self.c2 = c2
  }
}

extension _Concat3 : Sequence {
  struct Iterator : IteratorProtocol {
    var i0: C0.Iterator
    var i1: C1.Iterator
    var i2: C2.Iterator

    mutating func next() -> C0.Element? {
      if let r = i0.next() { return r }
      if let r = i1.next() { return r }
      return i2.next()
    }
  }

  func makeIterator() -> Iterator {
    return Iterator(
      i0: c0.makeIterator(),
      i1: c1.makeIterator(),
      i2: c2.makeIterator()
    )
  }
}

extension _Concat3 {
  public enum Index {
  case _0(C0.Index)
  case _1(C1.Index)
  case _2(C2.Index)
  }
}

extension _Concat3.Index : Comparable {
  static func == (lhs: _Concat3.Index, rhs: _Concat3.Index) -> Bool {
    switch (lhs, rhs) {
    case (._0(let l), ._0(let r)): return l == r
    case (._1(let l), ._1(let r)): return l == r
    case (._2(let l), ._2(let r)): return l == r
    default: return false
    }
  }
  
  static func < (lhs: _Concat3.Index, rhs: _Concat3.Index) -> Bool {
    switch (lhs, rhs) {
    case (._0, ._1), (._0, ._2), (._1, ._2): return true
    case (._1, ._0), (._2, ._0), (._2, ._1): return false
    case (._0(let l), ._0(let r)): return l < r
    case (._1(let l), ._1(let r)): return l < r
    case (._2(let l), ._2(let r)): return l < r
    }
  }
}

extension _Concat3 : Collection {
  var startIndex: Index {
    return !c0.isEmpty ? ._0(c0.startIndex)
         : !c1.isEmpty ? ._1(c1.startIndex) : ._2(c2.startIndex)
  }

  var endIndex: Index {
    return ._2(c2.endIndex)
  }

  func index(after i: Index) -> Index {
    switch i {
    case ._0(let j):
      let r = c0.index(after: j)
      if r != c0.endIndex { return ._0(r) }
      if !c1.isEmpty { return ._1(c1.startIndex) }
      return ._2(c2.startIndex)
      
    case ._1(let j):
      let r = c1.index(after: j)
      if r != c1.endIndex { return ._1(r) }
      return ._2(c2.startIndex)
      
    case ._2(let j):
      return ._2(c2.index(after: j))
    }
  }

  subscript(i: Index) -> C0.Element {
    switch i {
    case ._0(let j): return c0[j]
    case ._1(let j): return c1[j]
    case ._2(let j): return c2[j]
    }
  }
}

internal var zero_Int4 : Builtin.Int4 {
  return Builtin.trunc_Int32_Int4((0 as UInt32)._value)
}

internal var zero_Int120 : Builtin.Int120 {
  return Builtin.zext_Int32_Int120((0 as UInt32)._value)
}

extension String {
  public enum _Testable {}
}

extension String._Testable {
  // WORKAROUND: https://bugs.swift.org/browse/SR-5352
  // Using Builtin.Int120 bumps the size of the whole thing to 17 bytes!
  typealias _InlineStorage = (UInt64, UInt32, UInt16, UInt8)
  
  public // @testable
  struct _Inline<CodeUnit : FixedWidthInteger> {
    var _storage: _InlineStorage = (0,0,0,0)
    var _count: Builtin.Int4 = zero_Int4
  }

  public // @testable
  struct _Unowned<CodeUnit : FixedWidthInteger> {
    var _start: UnsafePointer<CodeUnit>
    var _count: UInt32
    var isASCII: Bool?
    var isNULTerminated: Bool
  }
}    

extension String {
  internal enum _Content {
    typealias _Inline<CodeUnit : FixedWidthInteger>
      = String._Testable._Inline<CodeUnit>
    typealias _Unowned<CodeUnit : FixedWidthInteger>
      = String._Testable._Unowned<CodeUnit>

  case inline8(_Inline<UInt8>)
    public var _inline8: _Inline<UInt8>?
    { if case .inline8(let x) = self { return x } else { return nil } }
     
  case inline16(_Inline<UInt16>)
    public var _inline16: _Inline<UInt16>?
    { if case .inline16(let x) = self { return x } else { return nil } }

  case unowned8(_Unowned<UInt8>)
    public var _unowned8: _Unowned<UInt8>?
    { if case .unowned8(let x) = self { return x } else { return nil } }

  case unowned16(_Unowned<UInt16>)
    public var _unowned16: _Unowned<UInt16>?
    { if case .unowned16(let x) = self { return x } else { return nil } }

  case latin1(_Latin1Storage)
    public var _latin1: _Latin1Storage?
    { if case .latin1(let x) = self { return x } else { return nil } }
    
  case utf16(_UTF16Storage)
    public var _utf16: _UTF16Storage?
    { if case .utf16(let x) = self { return x } else { return nil } }
    
  case nsString(_NSStringCore)
    public var _nsstring: _NSStringCore?
    { if case .nsString(let x) = self { return x } else { return nil } }
  }
}

extension String._Content._Inline {
  public var capacity: Int {
    return MemoryLayout.size(ofValue: _storage)
      / MemoryLayout<CodeUnit>.stride
  }

  internal var _bits : _UInt128 {
    get {
#if _endian(little)
      return unsafeBitCast(self, to: _UInt128.self) & ~(0 as _UInt128) &>> 8
#else
      return unsafeBitCast(self, to: _UInt128.self) &>> 8
#endif
    }
    set {
#if _endian(little)
      let non_bits = unsafeBitCast(self, to: _UInt128.self) & (0xFF &<< 120)
      self = unsafeBitCast(newValue | non_bits, to: type(of: self))
#else
      let non_bits = unsafeBitCast(self, to: _UInt128.self) & 0xFF
      self = unsafeBitCast((newValue &<< 8) | non_bits, to: type(of: self))
#endif
      _sanityCheck(_bits == newValue)
    }
  }
  
  public var count : Int {
    get {
      return Int(extendingOrTruncating: UInt8(Builtin.zext_Int4_Int8(_count)))
    }
    
    set {
      _count = Builtin.trunc_Int8_Int4(
        Int8(extendingOrTruncating: newValue)._value)
    }
  }
  
  public init?<S: Sequence>(_ s: S) where S.Element : BinaryInteger {
    var newBits: _UInt128 = 0
    var shift = 0
    let maxShift = MemoryLayout.size(ofValue: _storage) * 8
    for i in s {
      guard shift < maxShift, let _ = CodeUnit(exactly: i) else { return nil }
      newBits |= _UInt128(i) &<< shift
      shift += CodeUnit.bitWidth
    }
    count = shift / CodeUnit.bitWidth
    _bits = newBits
  }
}

extension String._Content._Inline : Sequence {
  public // @testable
  struct Iterator : IteratorProtocol, Sequence {
    @_versioned // @testable
    var bits: _UInt128
    public // @testable
    var count: UInt8

    @inline(__always)
    public // @testable
    mutating func next() -> CodeUnit? {
      guard count > 0 else { return nil }
      let r = CodeUnit(extendingOrTruncating: bits)
      bits &>>= CodeUnit.bitWidth
      count = count &- 1
      return r
    }
    public // @testable
    var underestimatedCount: Int { return Int(extendingOrTruncating: count) }
  }
  
  public func makeIterator() -> Iterator {
    return Iterator(bits: _bits, count: UInt8(extendingOrTruncating: count))
  }
}

extension String._Content._Inline : RandomAccessCollection, MutableCollection {
  public typealias Index = Int
  public var startIndex: Index { return 0 }
  public var endIndex: Index { return count }
  public subscript(i: Index) -> CodeUnit {
    get {
      return CodeUnit(extendingOrTruncating: _bits &>> (i &* CodeUnit.bitWidth))
    }
    set {
      let shift = i &* CodeUnit.bitWidth
      _bits = (_bits & ~(_UInt128(~newValue) &<< shift))
        | (_UInt128(newValue) &<< shift)
    }
  }
  public func index(after i: Index) -> Index {
    return i &+ 1
  }
  public func index(before i: Index) -> Index {
    return i &- 1
  }
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return i &+ n
  }
  public func distance(from i: Index, to j: Index) -> Int {
    return j &- i
  }
}

extension String._Content._Inline : CustomDebugStringConvertible {
  public var debugDescription: String {
    return String(describing: Array(self))
  }
}

extension String._Content._Inline {
  public mutating func append(_ u: CodeUnit) {
    let oldCount = count
    count = count &+ 1
    self[oldCount] = u
  }
}

extension String._Content._Inline where CodeUnit == UInt8 {
  internal var isASCII : Bool {
    return _bits & (0x80_8080__8080_8080___8080_8080__8080_8080 as _UInt128) == 0
  }
}

extension String._Content._Inline where CodeUnit == UInt16 {
  
  internal var isASCII : Bool {
    return _bits & (0xFF80__FF80_FF80___FF80_FF80__FF80_FF80 as _UInt128) == 0
  }
  
  internal var isLatin1 : Bool {
    return _bits & (0xFF00__FF00_FF00___FF00_FF00__FF00_FF00 as _UInt128) == 0
  }
}

extension String._Content._Unowned {
  init?(
    _ source: UnsafeBufferPointer<CodeUnit>,
    isASCII: Bool?,
    isNULTerminated: Bool
  ) {
    guard
      let count = UInt32(exactly: source.count),
      let start = source.baseAddress
    else { return nil }

    self._count = count
    self._start = start
    self.isASCII = isASCII
    self.isNULTerminated = isNULTerminated
  }

  public var unsafeBuffer: UnsafeBufferPointer<CodeUnit> {
    return UnsafeBufferPointer(start: _start, count: Int(_count))
  }
}

extension String._Content {

  init() {
    self = .inline16(_Inline<UInt16>(EmptyCollection<UInt16>())!)
  }
  
  var _existingLatin1 : UnsafeBufferPointer<UInt8>? {
    switch self {
    case .latin1(let x): return x.withUnsafeBufferPointer { $0 }
    case .unowned8(let x): return x.unsafeBuffer
      /*
    case .nsString(let x):
      return x._fastCStringContents(false).map {
        UnsafeBufferPointer(start: x, count: x.length())
      }
      */
    default: return nil
    }
  }

  var _existingUTF16 : UnsafeBufferPointer<UInt16>? {
    switch self {
    case .utf16(let x): return x.withUnsafeBufferPointer { $0 }
    case .unowned16(let x): return x.unsafeBuffer
    case .nsString(let x):
      return x._fastCharacterContents().map {
        UnsafeBufferPointer(start: $0, count: x.length())
      }
    default: return nil
    }
  }

  var isASCII: Bool? {
    get {
      switch self {
      case .inline8(let x): return x.isASCII
      case .inline16(let x): return x.isASCII 
      case .unowned8(let x): return x.isASCII
      case .unowned16(let x): return x.isASCII
      case .latin1(let x):  return x.isASCII
      case .utf16(let x): return x.isASCII
      case .nsString: return nil
      }
    }
  }
}

extension String._Testable {
  public // @testable
  struct UTF16View {
    @_versioned // testable
    var _content: String._Content
  }
}

extension String._Content {
  typealias UTF16View = String._Testable.UTF16View
  
  var _nsString : _NSStringCore {
    switch self {
    case .nsString(let x): return x
    case .utf16(let x): return x
    case .latin1(let x): return x
    default:
      _sanityCheckFailure("unreachable")
    }
  }
}

struct _TruncExt<Input: BinaryInteger, Output: FixedWidthInteger>
: _Function {
  func apply(_ input: Input) -> Output {
    return Output(extendingOrTruncating: input)
  }
}

extension String._Testable.UTF16View : Sequence {
  public struct Iterator : IteratorProtocol {
    public // @testable
    enum _Buffer {
    case deep8(UnsafePointer<UInt8>, UnsafePointer<UInt8>)
    case deep16(UnsafePointer<UInt16>, UnsafePointer<UInt16>)
    case inline8(String._Content._Inline<UInt8>.Iterator)
    case inline16(String._Content._Inline<UInt16>.Iterator)
    case nsString(Int)
    }

    public // @testable
    var _buffer: _Buffer
    internal let _owner: AnyObject?

    init(_ content: String._Content) {
      switch content {
      case .inline8(let x):
        _owner = nil
        _buffer = .inline8(x.makeIterator())
      case .inline16(let x):
        _owner = nil
        _buffer = .inline16(x.makeIterator())
      case .unowned8(let x):
        _owner = nil
        let b = x.unsafeBuffer
        let s = b.baseAddress._unsafelyUnwrappedUnchecked
        _buffer = _Buffer.deep8(s, s + b.count)
      case .unowned16(let x):
        _owner = nil
        let b = x.unsafeBuffer
        let s = b.baseAddress._unsafelyUnwrappedUnchecked
        _buffer = _Buffer.deep16(s, s + b.count)
      case .latin1(let x):
        _owner = x
        _buffer = x.withUnsafeBufferPointer {
          let s = $0.baseAddress._unsafelyUnwrappedUnchecked
          return .deep8(s, s + $0.count)
        }
      case .utf16(let x):
        _owner = x
        _buffer = x.withUnsafeBufferPointer {
          let s = $0.baseAddress._unsafelyUnwrappedUnchecked
          return .deep16(s, s + $0.count)
        }
      case .nsString(let x):
        _buffer = .nsString(0)
        _owner = x
      }
    }

    @inline(__always)
    init(_ content: String._Content, offset: Int) {
      switch content {
      case .inline8(let x):
        _owner = nil
        _buffer = .inline8(
          .init(
            bits: x._bits &>> (offset &<< 3),
            count: UInt8(extendingOrTruncating: x.count &- offset)))
      case .inline16(let x):
        _owner = nil
        _buffer = .inline16(
          .init(
            bits: x._bits &>> (offset &<< 4),
            count: UInt8(extendingOrTruncating: x.count &- offset)))
      case .utf16(let x):
        _owner = x
        _buffer = x.withUnsafeBufferPointer {
          let s = $0.baseAddress._unsafelyUnwrappedUnchecked
          return .deep16(s + offset, s + $0.count)
        }
      case .unowned16(let x):
        _owner = nil
        let b = x.unsafeBuffer
        let s = b.baseAddress._unsafelyUnwrappedUnchecked
        _buffer = _Buffer.deep16(s + offset, s + b.count)
      case .unowned8(let x):
        _owner = nil
        let b = x.unsafeBuffer
        let s = b.baseAddress._unsafelyUnwrappedUnchecked
        _buffer = _Buffer.deep8(s + offset, s + b.count)
      case .latin1(let x):
        _owner = x
        _buffer = x.withUnsafeBufferPointer {
          let s = $0.baseAddress._unsafelyUnwrappedUnchecked
          return .deep8(s + offset, s + $0.count)
        }
      case .nsString(let x):
        _buffer = .nsString(offset)
        _owner = x
      }
    }
    
    @inline(__always)
    public mutating func next() -> UInt16? {
      switch _buffer {
      case .inline8(var x):
        guard let r = x.next() else { return nil }
        _buffer = .inline8(x)
        return UInt16(r)
      case .inline16(var x):
        guard let r = x.next() else { return nil }
        _buffer = .inline16(x)
        return r
      case .deep8(let start, let end):
        guard start != end else { return nil }
        _buffer = .deep8(start + 1, end)
        return UInt16(start.pointee)
      case .deep16(let start, let end):
        guard start != end else { return nil }
        _buffer = .deep16(start + 1, end)
        return start.pointee
      case .nsString(let i):
        return _nextSlow(currentPosition: i)
      }
    }

    public // @testable
    mutating func _nextSlow(currentPosition i: Int) -> UInt16? {
      let s = unsafeBitCast(_owner, to: _NSStringCore.self)
      if i == s.length() { return nil }
      _buffer = .nsString(i + 1)
      return s.characterAtIndex(i)
      
    }
  }
  
  public func makeIterator() -> Iterator {
    return Iterator(_content)
  }

  // @inline(__always) - testable
  public func _copyContents(
    initializing destination: UnsafeMutableBufferPointer<Element>
  ) -> (Iterator, UnsafeMutableBufferPointer<Element>.Index) {
    var n = 0
    if var d = destination._position {
      n = destination._end._unsafelyUnwrappedUnchecked - d

      if case .inline8(let source) = _content, source.count <= n {
        n = source.count
        for u in source {
          d.pointee = UInt16(extendingOrTruncating: u)
          d += 1
        }
      }
      else if case .inline16(let source) = _content, source.count <= n {
        n = source.count
        for u in source {
          d.pointee = u
          d += 1
        }
      }
      else if let source = _content._existingUTF16 {
        let s = source._position._unsafelyUnwrappedUnchecked
        n = Swift.min(n, source._end._unsafelyUnwrappedUnchecked - s)
        d.initialize(from: s, count: n)
      }
      else if let source = _content._existingLatin1 {
        var s = source._position._unsafelyUnwrappedUnchecked
        n = Swift.min(n, source._end._unsafelyUnwrappedUnchecked - s)
        let end = d + n
        while d != end {
          d.pointee = UInt16(extendingOrTruncating: s.pointee)
          d += 1
          s += 1
        }
      }
      else {
        n = _copyContentsSlow(initializing: destination)
      }
    }
    return (Iterator(_content, offset: n), n)
  }

  @inline(never)
  func _copyContentsSlow(
    initializing destination: UnsafeMutableBufferPointer<Element>
  ) -> Int {
    var source = makeIterator()
    guard var p = destination.baseAddress else { return 0 }
    for n in 0..<destination.count {
      guard let x = source.next() else { return n }
      p.initialize(to: x)
      p += 1
    }
    return destination.count
  }
}

extension String._Content.UTF16View : BidirectionalCollection {
  public // @testable
  init<C : Collection>(
    _ c: C, maxElement: UInt16? = nil, minCapacity: Int = 0
  )
  where C.Element == UInt16 {
    if let x = String._Content._Inline<UInt8>(c) {
      _content = .inline8(x)
    }
    else if let x = String._Content._Inline<UInt16>(c) {
      _content = .inline16(x)
    }
    else  {
      let maxCodeUnit = maxElement ?? c.max() ?? 0
      if maxCodeUnit <= 0xFF {
        _content = .latin1(
            .copying(
              _MapCollection(c, through: _TruncExt()),
              minCapacity: minCapacity,
              isASCII: maxCodeUnit <= 0x7f))
      }
      else {
        _content = .utf16(
           .copying(c, minCapacity: minCapacity, maxElement: maxCodeUnit))
      }
    }
  }
  
  init<C : Collection>(
    _ c: C, minCapacity: Int = 0, isASCII: Bool? = nil
  ) where C.Element == UInt8 {
    if let x = String._Content._Inline<UInt8>(c) {
      _content = .inline8(x)
    }
    else {
      _content = .latin1(
          .copying(c, minCapacity: minCapacity, isASCII: isASCII))
    }
  }
  
  init(
    unowned source: UnsafeBufferPointer<UInt8>,
    isASCII: Bool?,
    isNULTerminated: Bool
  ) {
    if let x = String._Content._Inline<UInt8>(source) {
      _content = .inline8(x)
    }
    else if let x = String._Content._Unowned<UInt8>(
      source, isASCII: isASCII,
      isNULTerminated: isNULTerminated
    ) {
      _content = .unowned8(x)
    }
    else {
      _content = .latin1(.copying(source, isASCII: isASCII))
    }
  }
  
  init(
    unowned source: UnsafeBufferPointer<UInt16>,
    isASCII: Bool?,
    isNULTerminated: Bool
  ) {
    if let x = String._Content._Inline<UInt8>(source) {
      _content = .inline8(x)
    }
    else if let x = String._Content._Inline<UInt16>(source) {
      _content = .inline16(x)
    }
    else if let x = String._Content._Unowned<UInt16>(
      source, isASCII: isASCII,
      isNULTerminated: isNULTerminated
    ) {
      _content = .unowned16(x)
    }
    else if isASCII == true || !source.contains { $0 > 0xFF } {
      _content = .latin1(
        .copying(_MapCollection(source, through: _TruncExt()), isASCII: true))
    }
    else {
      _content = .utf16(.copying(source))
    }
  }
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return count }
  public var count: Int {
    // @inline(__always) - testable
    get {
      switch self._content {
      case .inline8(let x): return x.count
      case .inline16(let x): return x.count 
      case .unowned8(let x): return Int(x._count) 
      case .unowned16(let x): return Int(x._count) 
      case .latin1(let x):  return x.count 
      case .utf16(let x): return x.count 
      case .nsString(let x): return x.length() 
      }
    }
  }
  
  public subscript(i: Int) -> UInt16 {
    // @inline(__always) - testable
    get {
      switch self._content {
      case .inline8(var x): return UInt16(x[i])
      case .inline16(var x):  return x[i]
      case .unowned8(let x): return UInt16(x.unsafeBuffer[i])
      case .unowned16(let x): return x.unsafeBuffer[i]
      case .latin1(let x): return UInt16(x[i])
      case .utf16(let x): return x[i]
      case .nsString(let x):
        return x.characterAtIndex(i)
      }
    }
  }

  public func index(after i: Int) -> Int { return i + 1 }
  public func index(before i: Int) -> Int { return i - 1 }
}

extension String._Testable.UTF16View : RangeReplaceableCollection {
  public var capacity: Int {
    get {
      switch self._content {
      case .inline8(let x): return x.capacity
      case .inline16(let x): return x.capacity
      case .unowned8(let x): return Int(x._count)
      case .unowned16(let x): return Int(x._count)
      case .latin1(let x): return x.capacity
      case .utf16(let x): return x.capacity
      case .nsString(let x): return x.length()
      }
    }
  }
  
  public init() {
    _content = String._Content()
  }

  internal var _rangeReplaceableStorageID: ObjectIdentifier? {
    switch self._content {
    case .latin1(let x): return ObjectIdentifier(x)
    case .utf16(let x): return ObjectIdentifier(x)
    default: return nil
    }
  }

  internal var _dynamicStorageIsMutable: Bool? {
    mutating get {
      return _rangeReplaceableStorageID?._liveObjectIsUniquelyReferenced()
    }
  }

  /// Reserve space for appending `s`, gathering as much of the appropriate space
  /// as possible without consuming `s`.
  ///
  /// - Returns: `true` if `self` is known to have mutable capacity.
  @inline(__always)
  mutating func _reserveCapacity<S: Sequence>(forAppending s: S) -> Bool
  where S.Element == UInt16 {
    let growth = s.underestimatedCount
    guard growth > 0 else { return false }

    let minCapacity = count + growth

    var forceUTF16 = false

    // We have enough capacity and can write our storage
    if capacity >= minCapacity && _dynamicStorageIsMutable != false {
      // If our storage is already wide enough, we're done
      if case .utf16 = _content { return true }
      if case .inline16 = _content { return true }
      if (s._preprocessingPass { s.contains { $0 > 0xFF } } != true) {
        return true
      }
      // Otherwise, widen when reserving
      forceUTF16 = true
    }
    
    _allocateCapacity(
      Swift.max(minCapacity, 2 * count), forcingUTF16: forceUTF16)
    return true
  }

  @inline(__always)
  mutating func _allocateCapacity(_ minCapacity: Int, forcingUTF16: Bool) {
    if let codeUnits = _content._existingUTF16 {
      self._content = .utf16(
        String._UTF16Storage.copying(codeUnits, minCapacity: minCapacity))
    }
    else if !forcingUTF16, let codeUnits = _content._existingLatin1 {
      self._content = .latin1(
        String._Latin1Storage.copying(
          codeUnits, minCapacity: minCapacity, isASCII: _content.isASCII))
    }
    else {
      _allocateCapacitySlow(minCapacity, forcingUTF16: forcingUTF16)
    }
  }

  @inline(never)
  mutating func _allocateCapacitySlow(_ minCapacity: Int, forcingUTF16: Bool) {
    self._content = .utf16(
      String._UTF16Storage.copying(self, minCapacity: minCapacity))
  }
  
  public mutating func reserveCapacity(_ minCapacity: Int) {
    if capacity < minCapacity || _dynamicStorageIsMutable == false {
      _allocateCapacity(minCapacity, forcingUTF16: false)
    }
  }
  
  public mutating func append<S: Sequence>(contentsOf s: S)
  where S.Element == Element {
    let knownMutable = _reserveCapacity(forAppending: s)
    
    var source = s.makeIterator()
    defer { _fixLifetime(self) }

    switch _content {
    case .latin1(let x) where knownMutable || _dynamicStorageIsMutable != false:
      let buf = UnsafeMutableBufferPointer(
        start: x._baseAddress + x.count, count: x.capacity &- x.count)
      
      for i in 0..<buf.count {
        guard let u = source.next() else { break }
        guard u <= 0xFF else {
          self.append(u)
          break
        }
        buf[i] = UInt8(extendingOrTruncating: u)
        x.count = x.count &+ 1
      }

    case .utf16(let x) where knownMutable || _dynamicStorageIsMutable != false:
      let availableCapacity = UnsafeMutableBufferPointer(
        start: x._baseAddress + x.count, count: x.capacity &- x.count)

      var copiedCount = 0
      (source, copiedCount) = s._copyContents(initializing: availableCapacity)
      x.count += copiedCount

    case .inline8(var x):
      while x.count < x.capacity, let u = source.next() {
        guard let u8 = UInt8(exactly: u) else {
          _content = .inline8(x)
          self.append(u)
          break
        }
        x.append(u8)
      }
      
    case .inline16(var x):
      while x.count < x.capacity, let u = source.next() {
        x.append(u)
      }
      _content = .inline16(x)
      
    default:
      break
    }
    
    while let u = source.next() { append(u) }
  }

  public mutating func append(_ u: UInt16) {
    let knownUnique = _reserveCapacity(forAppending: CollectionOfOne(u))
    
    defer { _fixLifetime(self) }
    
    // In-place mutation
    if knownUnique || _dynamicStorageIsMutable != false {
      switch self._content {
      case .inline8(var x) where u <= 0xFF:
        x.append(UInt8(u))
        self._content = .inline8(x)
        return

      case .inline16(var x):
        x.append(u)
        self._content = .inline16(x)
        return

      case .latin1(let x) where u <= 0xFF:
        x.append(UInt8(u))
        return
        
      case .utf16(let x):
        x.append(u)
        return
        
      default: break
      }
      _replaceSubrangeSlow(
        endIndex..<endIndex, with: CollectionOfOne(u), maxNewElement: u)
    }
  }

  public mutating func replaceSubrange<C : Collection>(
    _ target: Range<Index>,
    with newElements_: C
  ) where C.Element == Element {
    defer { _fixLifetime(self) }

    let newElements = _Counted(newElements_)
    var maxNewElement: UInt16? = nil
    
    // In-place dynamic buffer
    if _dynamicStorageIsMutable == true {
      switch self._content {
      case .latin1(let x):
        maxNewElement = newElements.max() ?? 0
        if maxNewElement! <= 0xFF && x._tryToReplaceSubrange(
          target,
          with: _MapCollection(newElements, through: _TruncExt())
        ) {
          return
        }
      case .utf16(let x):
        if x._tryToReplaceSubrange(target, with: newElements) {
          return
        }
      default: break
      }
    }
    _replaceSubrangeSlow(
      target, with: newElements, maxNewElement: maxNewElement)
  }

  mutating func _replaceSubrangeSlow<C : Collection>(
    _ target: Range<Index>,
    with newElements: C,
      maxNewElement: UInt16?
  ) where C.Element == Element {
    let minCapacity
      = target.upperBound == count && !newElements.isEmpty ? count * 2 : count

    defer { _fixLifetime(self)  }
    
    if let codeUnits = _content._existingLatin1,
    (maxNewElement.map { $0 <= 0xFF } ?? !newElements.contains { $0 > 0xFF }) {
      self = .init(
        _Concat3(
          codeUnits[..<target.lowerBound],
          _MapCollection(newElements, through: _TruncExt()),
          codeUnits[target.upperBound...]),
        minCapacity: minCapacity
      )
    }
    else if let codeUnits = _content._existingUTF16 {
      self = .init(
        _Concat3(
          codeUnits[..<target.lowerBound],
          newElements,
          codeUnits[target.upperBound...]),
        minCapacity: minCapacity
      )
    }
    else {
      self = .init(
        _Concat3(
          self[..<target.lowerBound],
          newElements,
          self[target.upperBound...]),
        minCapacity: minCapacity
      )
    }
  }
}

extension String._Testable.UTF16View {
  public // @testable
  init(legacy source: _StringCore) {
    var isASCII: Bool? = nil
    
    defer { _fixLifetime(source) }
    if let x = String._Content._Inline<UInt8>(source) {
      _content = .inline8(x)
      return
    }
    else if let x = String._Content._Inline<UInt16>(source) {
      _content = .inline16(x)
      return
    }
    else if source._owner == nil {
      if let a = source.asciiBuffer {
        let base = a.baseAddress
        if let me = String._Content._Unowned<UInt8>(
          UnsafeBufferPointer<UInt8>(
            start: base, count: source.count),
          isASCII: true,
          isNULTerminated: true
        ) {
          _content = .unowned8(me)
          return
        }
      }
      else {
        isASCII = source.contains { $0 > 0x7f }
        if let me = String._Content._Unowned<UInt16>(
          UnsafeBufferPointer(
            start: source.startUTF16, count: source.count),
        isASCII: isASCII,
        isNULTerminated: true
        ) {
          _content = .unowned16(me)
          return
        }
      }
    }
    
    if isASCII == true || !source.contains { $0 > 0xff } {
      self = String._Testable.UTF16View(
        _MapCollection(source, through: _TruncExt()),
        isASCII: isASCII ?? false
      )
    }
    else {
      self = String._Testable.UTF16View(source)
    }
  }
}
