extension String {
  internal enum _XContent {
    internal struct _Inline<CodeUnit : FixedWidthInteger> {
      typealias _Storage = (UInt64, UInt32, UInt16)
      var _storage: _Storage
      var _count: UInt8
    }
  case inline8(_Inline<UInt8>)
  case inline16(_Inline<UInt16>)

    internal struct _Unowned<CodeUnit : FixedWidthInteger> {
      var _start: UnsafePointer<CodeUnit>
      var _count: UInt32
      var isKnownASCII: Bool
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
      / MemoryLayout<UInt8>.stride
  }

  @inline(__always)
  public init?<S: Sequence>(_ s: S) where S.Element : BinaryInteger {
    _storage = (0,0,0)
    _count = 0
    let failed: Bool = withUnsafeMutableBufferPointer {
      let start = $0.baseAddress._unsafelyUnwrappedUnchecked
      for i in s {
        guard _count < capacity, let u = CodeUnit(exactly: i)
        else { return true }
        start[Int(_count)] = u
        _count += 1
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
        UnsafeMutableBufferPointer(start: start, count: Int(_count)))
    }
  }

  @inline(__always)
  public func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<CodeUnit>)->R
  ) -> R {
    var storage = (_storage, 0 as CodeUnit)
    _sanityCheck(
      MemoryLayout<_Storage>.size + MemoryLayout<CodeUnit>.size
      == MemoryLayout.size(ofValue: storage)
    )
    return withUnsafePointer(to: &storage) {
      let start = UnsafeRawPointer($0).bindMemory(
        to: CodeUnit.self,
        capacity: capacity
      )
      return body(
        UnsafeBufferPointer(start: start, count: Int(_count)))
    }
  }
}

extension String._XContent._Inline where CodeUnit == UInt8 {
  internal var isKnownASCII : Bool {
    return (_storage.0 | UInt64(_storage.1) | UInt64(_storage.2))
      & (0x8080_8080__8080_8080 as UInt64).littleEndian == 0
  }
}

extension String._XContent._Inline where CodeUnit == UInt16 {
  
  internal var isKnownASCII : Bool {
    return (_storage.0 | UInt64(_storage.1) | UInt64(_storage.2))
      & (0xFF80_FF80__FF80_FF80 as UInt64).littleEndian == 0
  }
  
  internal var isKnownLatin1 : Bool {
    return (_storage.0 | UInt64(_storage.1) | UInt64(_storage.2))
      & (0xFF00_FF00__FF00_FF00 as UInt64).littleEndian == 0
  }
  
}

extension String._XContent._Unowned {
  @inline(__always)
  init?(
    _ source: UnsafeBufferPointer<CodeUnit>,
    isKnownASCII: Bool,
    isNULTerminated: Bool
  ) {
    guard
      let count = UInt32(exactly: source.count),
      let start = source.baseAddress
    else { return nil }

    self._count = count
    self._start = start
    self.isKnownASCII = isKnownASCII
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

extension String._XContent.UTF16View : BidirectionalCollection {
  init<C : Collection>(_ c: C) where C.Element == UInt16 {
    if let x = String._XContent._Inline<UInt16>(c) {
      _content = .inline16(x)
    }
    else {
      _content = .utf16(//.init(c)
        _mkUTF16(c) as! String._UTF16Storage
      )
    }
  }
  
  init<C : Collection>(_ c: C) where C.Element == UInt8 {
    if let x = String._XContent._Inline<UInt8>(c) {
      _content = .inline8(x)
    }
    else {
      _content = .latin1(//.init(c)
        _mkLatin1(c) as! String._Latin1Storage)
    }
  }
  
  init(
    unowned source: UnsafeBufferPointer<UInt8>,
    isKnownASCII: Bool,
    isNULTerminated: Bool
  ) {
    if let x = String._XContent._Inline<UInt8>(source) {
      _content = .inline8(x)
    }
    else if let x = String._XContent._Unowned<UInt8>(
      source, isKnownASCII: isKnownASCII,
      isNULTerminated: isNULTerminated
    ) {
      _content = .unowned8(x)
    }
    else {
      _content = .latin1(//.init(c)
      _mkLatin1(source, isKnownASCII: isKnownASCII)
        as! String._Latin1Storage)
    }
  }
  
  init(
    unowned source: UnsafeBufferPointer<UInt16>,
    isKnownASCII: Bool,
    isNULTerminated: Bool
  ) {
    if let x = String._XContent._Inline<UInt8>(source) {
      _content = .inline8(x)
    }
    else if let x = String._XContent._Inline<UInt16>(source) {
      _content = .inline16(x)
    }
    else if let x = String._XContent._Unowned<UInt16>(
      source, isKnownASCII: isKnownASCII,
      isNULTerminated: isNULTerminated
    ) {
      _content = .unowned16(x)
    }
    else if isKnownASCII {
      _content = .latin1(
        _mkLatin1(
          source.lazy.map { UInt8(extendingOrTruncating: $0) },
          isKnownASCII: true) as! String._Latin1Storage)
    }
    else {
      _content = .utf16(//.init(c)
        _mkUTF16(source, isKnownASCII: isKnownASCII)
        as! String._UTF16Storage)
    }
  }
  
  var startIndex: Int { return 0 }
  var endIndex: Int { return count }
  var count: Int {
    @inline(__always)
    get {
      switch self._content {
      case .inline8(let x): return Int(x._count) 
      case .inline16(let x): return Int(x._count) 
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
    defer { _fixLifetime(source) }
    let isASCII = !source.contains { $0 >= 0x80 }
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
            UnsafeBufferPointer<UInt8>(start: base, count: source.count),
            isKnownASCII: isASCII,
          isNULTerminated: true) {
          _content = .unowned8(me)
          return
        }
      }
      else if let me = String._XContent._Unowned<UInt16>(
        UnsafeBufferPointer(start: source.startUTF16, count: source.count),
        isKnownASCII: isASCII,
        isNULTerminated: true
      ) {
        _content = .unowned16(me)
        return
      }
    }
    
    if isASCII {
      self = String._XContent.UTF16View(
        source.lazy.map { UInt8(extendingOrTruncating: $0) })
    }
    else {
      self = String._XContent.UTF16View(source)
    }
  }
}

let testers: [String] = [
  "foo", "foobar", "foobarbaz", "the quick brown fox",
  "ƒoo", "ƒoobar", "ƒoobarba", "the quick brown ƒox"
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

  let contents = cores.map {
    String._XContent.UTF16View(legacy: $0)
  }


  /*
  for (x, y) in zip(cores, contents) {
    assert(x.elementsEqual(y))
    debugPrint(String(x))
    dump(y)
    print()
  }
  */
  
  var total = 0
  time {
    for x in 0...100000 {
      for a in contents {
        for b in contents {
          if a.lexicographicallyPrecedes(b) { total = total &+ 1 }
        }
      }
    }
  }

  time {
    for x in 0...100000 {
      for a in cores {
        for b in cores {
          if a.lexicographicallyPrecedes(b) { total = total &+ 1 }
        }
      }
    }
  }

  if total == 0 { print() }
}

assert(MemoryLayout<String._XContent>.size == 16)
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

