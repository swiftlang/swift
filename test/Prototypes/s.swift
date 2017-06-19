import Swift

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
    @inline(__always)
    get {
#if arch(i386) || arch(arm)
      return Int(Builtin.zext_Int4_Int32(_count))
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)      
      return Int(Builtin.zext_Int4_Int64(_count))
#endif
    }
    
    @inline(__always)
    set {
#if arch(i386) || arch(arm)
      _count = Builtin.trunc_Int32_Int4(newValue._value)
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)      
      _count = Builtin.trunc_Int64_Int4(newValue._value)
#endif
    }
  }
  
  @inline(__always)
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

  @inline(__always)
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

  @inline(__always)
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
  @inline(__always)
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

  @inline(__always)
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<CodeUnit>)->R
  ) -> R {
    return body(
      UnsafeBufferPointer(start: _start, count: Int(_count)))
  }
}

extension String._XContent {
  @inline(__always)
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
    default:
      return nil
    }
  }

  @inline(__always)
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
  @inline(__always)
  init<C : Collection>(_ c: C, maxElement: UInt16? = nil)
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
              isKnownASCII: maxCodeUnit <= 0x7f),
            to: String._Latin1Storage.self))
      }
      else {
        _content = .utf16(//.init(c)
          unsafeDowncast(
            _mkUTF16(c, maxElement: maxCodeUnit),
            to: String._UTF16Storage.self))
      }
    }
  }
  
  @inline(__always)
  init<C : Collection>(_ c: C, isASCII: Bool? = nil)
  where C.Element == UInt8 {
    if let x = String._XContent._Inline<UInt8>(c) {
      _content = .inline8(x)
    }
    else {
      _content = .latin1(//.init(c)
        unsafeDowncast(
          _mkLatin1(c, isKnownASCII: isASCII ?? false),
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
          _mkLatin1(source, isKnownASCII: isASCII ?? false)
          , to: String._Latin1Storage.self))
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
            isKnownASCII: true),
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
      /*
      return _content._withExistingLatin1Buffer { $0.count }
      ?? _content._withExistingUTF16Buffer { $0.count }
      ?? _content._nsString.length()
      */
      
    }
  }
  
  subscript(i: Int) -> UInt16 {
    @inline(__always)
    get {
      switch self._content {
      case .inline8(let x):
        return x.withUnsafeBufferPointer { UInt16($0[i]) }
      case .inline16(let x):
        return x.withUnsafeBufferPointer { $0[i] }
      case .unowned8(let x):
        return x.withUnsafeBufferPointer { UInt16($0[i]) }
      case .unowned16(let x): 
        return x.withUnsafeBufferPointer { $0[i] }
      case .latin1(let x):
        return UInt16(x[i])
      case .utf16(let x):
        return x[i]
      case .nsString(let x):
        return x.characterAtIndex(i) 
      }
    }
  }

  func index(after i: Int) -> Int { return i + 1 }
  func index(before i: Int) -> Int { return i - 1 }
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
  for (x, y) in zip(cores, contents) {
    if !x.elementsEqual(y) { fatalError("unequal") }
    _sanityCheck(
      {
        N = 1
        debugPrint(String(x))
        dump(y)
        print()
        return true
      }())
  }

  var total = 0
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
  lex_new()

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

  func init_new() {
    time {
      for _ in 0...10*N {
        for a in arrays {
          total = total &+ String._XContent.UTF16View(a).count
        }
      }
    }
  }
  init_new()
  
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
  
  if total == 0 { print() }
}

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

