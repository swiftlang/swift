import Swift

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
      if _fastPath(r != c0.endIndex) { return ._0(r) }
      if !c1.isEmpty { return ._1(c1.startIndex) }
      return ._2(c2.startIndex)
      
    case ._1(let j):
      let r = c1.index(after: j)
      if _fastPath(r != c1.endIndex) { return ._1(r) }
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

extension String {
  internal enum _XContent {
    internal struct _Inline<CodeUnit : FixedWidthInteger> {
      typealias _Storage = (UInt64, UInt32, UInt16, UInt8)
      var _storage: _Storage
#if arch(i386) || arch(arm)
      var _count: Builtin.Int4 = Builtin.trunc_Int32_Int4(0._value)
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)      
      var _count: Builtin.Int4 = Builtin.trunc_Int64_Int4(0._value)
#endif
    }
  case inline8(_Inline<UInt8>)
  case inline16(_Inline<UInt16>)

    internal struct _Unowned<CodeUnit : FixedWidthInteger> {
      var _start: UnsafePointer<CodeUnit>
      var _count: UInt32
      var isASCII: Bool?
      var isNULTerminated: Bool
    }
    
  case unowned8(_Unowned<UInt8>)
  case unowned16(_Unowned<UInt16>)
  case latin1(_Latin1Storage)
  case utf16(_UTF16Storage)
  case nsString(_NSStringCore)
  }
}

extension String._XContent._Inline {
  public var capacity: Int {
    return MemoryLayout.size(ofValue: _storage)
      / MemoryLayout<CodeUnit>.stride
  }

  public var count : Int {
    get {
#if arch(i386) || arch(arm)
      return Int(Builtin.zext_Int4_Int32(_count))
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)      
      return Int(Builtin.zext_Int4_Int64(_count))
#endif
    }
    
    set {
#if arch(i386) || arch(arm)
      _count = Builtin.trunc_Int32_Int4(newValue._value)
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)      
      _count = Builtin.trunc_Int64_Int4(newValue._value)
#endif
    }
  }
  
  public init?<S: Sequence>(_ s: S) where S.Element : BinaryInteger {
    _storage = (0,0,0,0)
    let failed: Bool = withUnsafeMutableBufferPointer {
      let start = $0.baseAddress._unsafelyUnwrappedUnchecked
      for i in s {
        guard count < capacity, let u = CodeUnit(exactly: i)
        else { return true }
        start[count] = u
        count = count &+ 1
      }
      return false
    }
    if failed { return nil }
  }

  public mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<CodeUnit>)->R
  ) -> R {
    return withUnsafeMutablePointer(to: &_storage) {
      let start = UnsafeMutableRawPointer($0).bindMemory(
        to: CodeUnit.self,
        capacity: capacity
      )
      return body(
        UnsafeMutableBufferPointer(start: start, count: count))
    }
  }

  /// Calls `body` on a mutable buffer that covers the entire extent of
  /// allocated memory.
  public mutating func _withMutableCapacity<R>(
    body: (inout UnsafeMutableBufferPointer<CodeUnit>)->R
  ) -> R {
    return self.withUnsafeMutableBufferPointer { buf in
      var fullBuf = UnsafeMutableBufferPointer(
        start: buf.baseAddress, count: capacity)
      return body(&fullBuf)
    }
  }
  
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<CodeUnit>)->R
  ) -> R {
    var storage = (_storage, 0 as UInt8)
    return withUnsafePointer(to: &storage) {
      let start = UnsafeRawPointer($0).bindMemory(
        to: CodeUnit.self,
        capacity: capacity
      )
      _sanityCheck(start[count] == 0)
      return body(
        UnsafeBufferPointer(start: start, count: count))
    }
  }

  public mutating func append(_ u: CodeUnit) {
    let oldCount = count
    count = count &+ 1
    withUnsafeMutableBufferPointer { $0[oldCount] = u }
  }
}

extension String._XContent._Inline where CodeUnit == UInt8 {
  internal var isASCII : Bool {
    return (UInt64(_storage.0) | UInt64(_storage.1) | UInt64(_storage.2))
      & (0x8080_8080__8080_8080 as UInt64).littleEndian == 0
  }
}

extension String._XContent._Inline where CodeUnit == UInt16 {
  
  internal var isASCII : Bool {
    return (UInt64(_storage.0) | UInt64(_storage.1) | UInt64(_storage.2))
      & (0xFF80_FF80__FF80_FF80 as UInt64).littleEndian == 0
  }
  
  internal var isLatin1 : Bool {
    return (UInt64(_storage.0) | UInt64(_storage.1) | UInt64(_storage.2))
      & (0xFF00_FF00__FF00_FF00 as UInt64).littleEndian == 0
  }
  
}

extension String._XContent._Unowned {
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

  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<CodeUnit>)->R
  ) -> R {
    return body(
      UnsafeBufferPointer(start: _start, count: Int(_count)))
  }
}

extension String._XContent {

  init() {
    self = .inline16(_Inline<UInt16>(EmptyCollection<UInt16>())!)
  }
  
  func _withExistingLatin1Buffer<R>(
    _ body: (UnsafeBufferPointer<UInt8>) -> R
  ) -> R? {
    switch self {
    case .inline8(let x):
      _onFastPath()
      return x.withUnsafeBufferPointer(body)
    case .latin1(let x):
      _onFastPath()
      return x.withUnsafeBufferPointer(body)
    case .unowned8(let x):
      return x.withUnsafeBufferPointer(body)
    case .inline16(let x):
      guard x.count == 0 else { return nil }
      return x.withUnsafeBufferPointer {
        $0.baseAddress!.withMemoryRebound(
          to: UInt8.self, capacity: 1
        ) {
          body(UnsafeBufferPointer(start: $0, count: 0))
        }
      }
    default:
      return nil
    }
  }

  func _withExistingUTF16Buffer<R>(
    _ body: (UnsafeBufferPointer<UInt16>) -> R
  ) -> R? {
    switch self {
    case .inline16(let x):
      _onFastPath()
      return x.withUnsafeBufferPointer(body)
    case .utf16(let x):
      _onFastPath()
      return x.withUnsafeBufferPointer(body)
    case .unowned16(var x):
      return x.withUnsafeBufferPointer(body)
    case .nsString(let x):
      defer { _fixLifetime(x) }
      return x._fastCharacterContents().map {
        body(UnsafeBufferPointer(start: $0, count: x.length()))
      }
    default:
      return nil
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

extension String._XContent {
  struct UTF16View {
    var _content: String._XContent
  }
  
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

extension String._XContent.UTF16View : BidirectionalCollection {
  init<C : Collection>(
    _ c: C, maxElement: UInt16? = nil, minCapacity: Int = 0
  )
  where C.Element == UInt16 {
    if let x = String._XContent._Inline<UInt8>(c) {
      _content = .inline8(x)
    }
    else if let x = String._XContent._Inline<UInt16>(c) {
      _content = .inline16(x)
    }
    else  {
      let maxCodeUnit = maxElement ?? c.max() ?? 0
      if maxCodeUnit <= 0xFF {
        _content = .latin1(
          unsafeDowncast(
            _mkLatin1(
              _MapCollection(c, through: _TruncExt()),
              minCapacity: minCapacity,
              isASCII: maxCodeUnit <= 0x7f),
            to: String._Latin1Storage.self))
      }
      else {
        _content = .utf16(//.init(c)
          unsafeDowncast(
            _mkUTF16(
              c,
              minCapacity: minCapacity,
              maxElement: maxCodeUnit),
            to: String._UTF16Storage.self))
      }
    }
  }
  
  init<C : Collection>(
    _ c: C, minCapacity: Int = 0, isASCII: Bool? = nil
  ) where C.Element == UInt8 {
    if let x = String._XContent._Inline<UInt8>(c) {
      _content = .inline8(x)
    }
    else {
      _content = .latin1(//.init(c)
        unsafeDowncast(
          _mkLatin1(c, minCapacity: minCapacity, isASCII: isASCII),
          to: String._Latin1Storage.self))
    }
  }
  
  init(
    unowned source: UnsafeBufferPointer<UInt8>,
    isASCII: Bool?,
    isNULTerminated: Bool
  ) {
    if let x = String._XContent._Inline<UInt8>(source) {
      _content = .inline8(x)
    }
    else if let x = String._XContent._Unowned<UInt8>(
      source, isASCII: isASCII,
      isNULTerminated: isNULTerminated
    ) {
      _content = .unowned8(x)
    }
    else {
      _content = .latin1(//.init(c)
        unsafeDowncast(
          _mkLatin1(source, isASCII: isASCII),
          to: String._Latin1Storage.self))
    }
  }
  
  init(
    unowned source: UnsafeBufferPointer<UInt16>,
    isASCII: Bool?,
    isNULTerminated: Bool
  ) {
    if let x = String._XContent._Inline<UInt8>(source) {
      _content = .inline8(x)
    }
    else if let x = String._XContent._Inline<UInt16>(source) {
      _content = .inline16(x)
    }
    else if let x = String._XContent._Unowned<UInt16>(
      source, isASCII: isASCII,
      isNULTerminated: isNULTerminated
    ) {
      _content = .unowned16(x)
    }
    else if isASCII == true || !source.contains { $0 > 0xFF } {
      _content = .latin1(
        unsafeDowncast(
          _mkLatin1(
            _MapCollection(source, through: _TruncExt()),
            isASCII: true),
          to: String._Latin1Storage.self))
    }
    else {
      _content = .utf16(//.init(c)            
        unsafeDowncast(
          _mkUTF16(source), to: String._UTF16Storage.self))
    }
  }
  
  var startIndex: Int { return 0 }
  var endIndex: Int { return count }
  var count: Int {
    @inline(__always)
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
  
  subscript(i: Int) -> UInt16 {
    @inline(__always)
    get {
      switch self._content {
      case .inline8(let x): return x.withUnsafeBufferPointer { UInt16($0[i]) }
      case .inline16(let x): return x.withUnsafeBufferPointer { $0[i] }
      case .unowned8(let x): return x.withUnsafeBufferPointer { UInt16($0[i]) }
      case .unowned16(let x): return x.withUnsafeBufferPointer { $0[i] }
      case .latin1(let x): return UInt16(x[i])
      case .utf16(let x): return x[i]
      case .nsString(let x):
        return x.characterAtIndex(i) 
      }
    }
  }

  func index(after i: Int) -> Int { return i + 1 }
  func index(before i: Int) -> Int { return i - 1 }
}

extension String._XContent.UTF16View : RangeReplaceableCollection {
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
    _content = String._XContent()
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
      if _content._withExistingLatin1Buffer({ _ in () }) == nil { return true }
      if (s._preprocessingPass { s.max() ?? 0 } ?? 0) <= 0xFF { return true }
      // Otherwise, widen when reserving
      forceUTF16 = true
    }
    
    _allocateCapacity(
      Swift.max(minCapacity, 2 * count), forcingUTF16: forceUTF16)
    return true
  }

  mutating func _allocateCapacity(_ minCapacity: Int, forcingUTF16: Bool) {
    if let _ = _content._withExistingLatin1Buffer({ buf in
        if !forcingUTF16 {
          self._content = .latin1(
            String._Latin1Storage.copying(
              buf, minCapacity: minCapacity, isASCII: _content.isASCII))
        }
        else {
          self._content = .utf16(
            String._UTF16Storage.copying(
              _MapCollection(buf, through: _TruncExt()),
              minCapacity: minCapacity,
              maxElement: _content.isASCII == true ? 0x7F
              : _content.isASCII == false ? 0xFF : nil)
          )
        }
      }) { return }

    if let _ = _content._withExistingUTF16Buffer({ buf in
        self._content = .utf16(
          String._UTF16Storage.copying(buf, minCapacity: minCapacity))
      }) { return }

    self._content = .utf16(
      String._UTF16Storage.copying(self, minCapacity: minCapacity))
  }
  
  mutating func reserveCapacity(_ minCapacity: Int) {
    if capacity < minCapacity || _dynamicStorageIsMutable == false {
      _allocateCapacity(minCapacity, forcingUTF16: false)
    }
  }
  
  mutating func append<S: Sequence>(contentsOf source_: S)
  where S.Element == Element {
    let knownMutable = _reserveCapacity(forAppending: source_)
    
    var source = IteratorSequence(source_.makeIterator())
    defer { _fixLifetime(self) }

    switch _content {
    case .inline8(var x):
      x._withMutableCapacity { buf in
        for i in count..<buf.count {
          let u = source.next()
          guard _fastPath(u != nil && u! <= 0xFF) else {
            let newContent = String._XContent._Inline<UInt8>(buf[..<i])!
            _content = .inline8(newContent)
            if u != nil { self.append(u!) }
            break
          }
          buf[i] = UInt8(extendingOrTruncating: u!)
        }
      }
      
    case .latin1(let x) where knownMutable || _dynamicStorageIsMutable != false:
      x._withMutableCapacity { buf in
        for i in count..<buf.count {
          guard let u = source.next() else { break }
          guard _fastPath(u <= 0xFF) else {
            self.append(u)
            break
          }
          buf[i] = UInt8(extendingOrTruncating: u)
          x.count += 1
        }
      }
      
    case .inline16(var x):
      x._withMutableCapacity { buf in
        for i in count..<buf.count {
          let u = source.next()
          guard _fastPath(u != nil) else {
            _content = .inline16(String._XContent._Inline<UInt16>(buf[..<i])!)
            break
          }
          buf[i] = u!
        }
      }
      
    case .utf16(let x) where knownMutable || _dynamicStorageIsMutable != false:
      x._withMutableCapacity { buf in
        var availableCapacity = UnsafeMutableBufferPointer(
          start: buf.baseAddress._unsafelyUnwrappedUnchecked + x.count,
          count: buf.count - x.count)
        let (newSource, copiedCount) = source._copyContents(
          initializing: availableCapacity
        )
        x.count += copiedCount
        source = newSource
      }
    default: break
    }
    for u in source { append(u) }
  }

  mutating func append(_ u: UInt16) {
    let knownUnique = _reserveCapacity(forAppending: CollectionOfOne(u))
    
    defer { _fixLifetime(self) }
    
    // In-place mutation
    if _fastPath(knownUnique || _dynamicStorageIsMutable != false) {
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

  mutating func replaceSubrange<C : Collection>(
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
    var done: Bool = false
    
    _content._withExistingLatin1Buffer {
      b in
      guard maxNewElement ?? newElements.max() ?? 0 <= 0xFF
      else { return }
      
      self = .init(
        _Concat3(
          b[..<target.lowerBound],
          _MapCollection(newElements, through: _TruncExt()),
          b[target.upperBound...]),
        minCapacity: count * 2
      )
      done = true
    }
    if done { return }

    _content._withExistingUTF16Buffer {
      b in
      self = .init(
        _Concat3(
          b[..<target.lowerBound],
          newElements,
          b[target.upperBound...]),
        minCapacity: count * 2
      )
      done = true
    }
    if done { return }
    
    self = .init(
      _Concat3(
        self[..<target.lowerBound],
        newElements,
        self[target.upperBound...]),
      minCapacity: count * 2
    )
  }
}

extension String._XContent.UTF16View {
  init(legacy source: _StringCore) {
    var isASCII: Bool? = nil
    
    defer { _fixLifetime(source) }
    if let x = String._XContent._Inline<UInt8>(source) {
      _content = .inline8(x)
      return
    }
    else if let x = String._XContent._Inline<UInt16>(source) {
      _content = .inline16(x)
      return
    }
    else if source._owner == nil {
      if let a = source.asciiBuffer {
        let base = a.baseAddress
        if let me = String._XContent._Unowned<UInt8>(
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
        if let me = String._XContent._Unowned<UInt16>(
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
      self = String._XContent.UTF16View(
        _MapCollection(source, through: _TruncExt()),
        isASCII: isASCII ?? false
      )
    }
    else {
      self = String._XContent.UTF16View(source)
    }
  }
}

let testers: [String] = [
  "foo", "foobar", "foobarbaz", "foobarbazniz", "foobarbaznizman", "the quick brown fox",
  "f\u{f6}o", "f\u{f6}obar", "f\u{f6}obarbaz", "f\u{f6}obarbazniz", "f\u{f6}obarbaznizman", "the quick br\u{f6}wn fox",
  "ƒoo", "ƒoobar", "ƒoobarba", "ƒoobarbazniz", "ƒoobarbaznizman", "the quick brown ƒox"
]

import Dispatch
import Darwin

func time<T>(_ _caller : String = #function, _ block: () -> T) -> T {
  let start = DispatchTime.now()
  let res = block()
  let end = DispatchTime.now()
  let milliseconds = (Double(end.uptimeNanoseconds) - Double(start.uptimeNanoseconds)) / 1_000_000.0
  print("\(_caller),\(milliseconds)")        
  return res
}


func testme2() {
  let cores
  = testers.map { $0._core } + testers.map { ($0 + "X")._core }

  let arrays = cores.map(Array.init)
  
  let contents = cores.map {
    String._XContent.UTF16View(legacy: $0)
  }

  var N = 10000
  _sanityCheck({ N = 1; return true }()) // Reset N for debug builds
  
  for (x, y) in zip(cores, contents) {
    if !x.elementsEqual(y) { fatalError("unequal") }
    _sanityCheck(
      {
        debugPrint(String(x))
        dump(y)
        print()
        return true
      }())
  }

  var total = 0
  @inline(never)
  func lex_new() {
    time {
      for _ in 0...N {
        for a in contents {
          for b in contents {
            if a.lexicographicallyPrecedes(b) { total = total &+ 1 }
          }
        }
      }
    }
  }

  @inline(never)
  func lex_old() {
    time {
      for _ in 0...N {
        for a in cores {
          for b in cores {
            if a.lexicographicallyPrecedes(b) { total = total &+ 1 }
          }
        }
      }
    }
  }
  lex_old()
  lex_new()
  print()
  
  @inline(never)
  func init_new() {
    time {
      for _ in 0...10*N {
        for a in arrays {
          total = total &+ String._XContent.UTF16View(a).count
        }
      }
    }
  }
  
  @inline(never)
  func init_old() {
    time {
      for _ in 0...10*N {
        for a in arrays {
          total = total &+ _StringCore(a).count
        }
      }
    }
  }
  init_old()
  init_new()
  print()
  
  @inline(never)
  func concat3Iteration() {
    time {
      for _ in 0...100*N {
        for x in _Concat3(5..<90, 6...70, (4...30).dropFirst()) {
          total = total &+ x
        }
      }
    }
  }
  concat3Iteration()
  print()
  
  let a_old = "a"._core
  let a_new = String._XContent.UTF16View(a_old)
  
  let short8_old = ["b","c","d","pizza"].map { $0._core }
  let short8_new = short8_old.map { String._XContent.UTF16View($0) }
  
  @inline(never)
  func  buildString_old() {
    time {
      var sb = a_old
      for _ in 0...N*200 {
        for x in short8_old {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  buildString_old()
  
  @inline(never)
  func  buildString_new() {
    time {
      var sb = a_new
      for _ in 0...N*200 {
        for x in short8_new {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  buildString_new()
  print()
  
  let short16_old = ["🎉","c","d","pizza"].map { $0._core }
  let short16_new = short16_old.map { String._XContent.UTF16View($0) }

  @inline(never)
  func  buildStringUTF16_old() {
    time {
      var sb = a_old
      for _ in 0...N*300 {
        for x in short16_old {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  buildStringUTF16_old()
  
  @inline(never)
  func  buildStringUTF16_new() {
    time {
      var sb = a_new
      for _ in 0...N*300 {
        for x in short16_new {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  buildStringUTF16_new()
  print()
  
  let ghost_old = "👻"._core
  let ghost_new = String._XContent.UTF16View(ghost_old)
  
  let long_old = "Swift is a multi-paradigm, compiled programming language created for iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is designed to work with Apple's Cocoa and Cocoa Touch frameworks and the large body of existing Objective-C code written for Apple products. Swift is intended to be more resilient to erroneous code (\"safer\") than Objective-C and also more concise. It is built with the LLVM compiler framework included in Xcode 6 and later and uses the Objective-C runtime, which allows C, Objective-C, C++ and Swift code to run within a single program."._core
  let long_new = String._XContent.UTF16View(long_old)
  
  @inline(never)
  func  buildStringLong_old() {
    time {
      var sb = ghost_old
      for _ in 0...N*20 {
        sb.append(contentsOf: long_old)
      }
      total = total &+ sb.count
    }
  }
  buildStringLong_old()
  
  @inline(never)
  func  buildStringLong_new() {
    time {
      var sb = ghost_new
      for _ in 0...N*20 {
        sb.append(contentsOf: long_new)
      }
      total = total &+ sb.count
    }
  }
  buildStringLong_new()
  print()
  
  @inline(never)
  func  buildShortString_old() {
    time {
      for _ in 0...N*200 {
        var sb = a_old
        for x in short8_old {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  buildShortString_old()
  
  @inline(never)
  func  buildShortString_new() {
    time {
      for _ in 0...N*200 {
        var sb = a_new
        for x in short8_new {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  buildShortString_new()
  print()
  
  @inline(never)
  func  buildShortStringUTF16_old() {
    time {
      for _ in 0...N*300 {
        var sb = a_old
        for x in short16_old {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  buildShortStringUTF16_old()
  
  @inline(never)
  func  buildShortStringUTF16_new() {
    time {
      for _ in 0...N*300 {
        var sb = a_new
        for x in short16_new {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  buildShortStringUTF16_new()
  print()
  
  @inline(never)
  func  buildShortStringLong_old() {
    time {
      for _ in 0...N*20 {
        var sb = ghost_old
        sb.append(contentsOf: long_old)
        total = total &+ sb.count
      }
    }
  }
  buildShortStringLong_old()
  
  @inline(never)
  func  buildShortStringLong_new() {
    time {
      for _ in 0...N*20 {
        var sb = ghost_new
        sb.append(contentsOf: long_new)
      }
    }
  }
  buildShortStringLong_new()
  print()
  
  if total == 0 { print() }
}

let cat = _Concat3(5..<10, 15...20, (25...30).dropFirst())
print(Array(cat))
print(cat.indices.map { cat[$0] })
print(MemoryLayout<String._XContent>.size)
assert(MemoryLayout<String._XContent>.size <= 16)
testme2()


/*
let samples = (0...1000000000).map {
  _ in UInt8(extendingOrTruncating: arc4random())
}

@inline(never)
func mix(_ x: [UInt8]) -> UInt8 {
  return x.max() ?? 0
}

@inline(never)
func mask(_ x: [UInt8]) -> UInt8 {
  return x.reduce(0) { $0 | $1 }
}


_ = time { max(samples) }
_ = time { mask(samples) }
*/

