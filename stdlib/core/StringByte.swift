struct StringByte : BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value : Builtin.Int8

  static func _convertFromBuiltinIntegerLiteral(val : Builtin.Int128) -> StringByte {
    return StringByte(Builtin.trunc_Int128_Int8(val))
  }

  typealias IntegerLiteralType = StringByte
  static func convertFromIntegerLiteral(value : StringByte) -> StringByte {
    return value
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : StringByte { get: return 0xFF }
  // static var min : StringByte { get: return 0 }
  static func max() -> StringByte { return 0xFF }
  static func min() -> StringByte { return 0 }
}

func + (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.add_Int8(lhs.value, rhs.value))
}

func - (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.sub_Int8(lhs.value, rhs.value))
}

extension StringByte : Comparable, Hashable {
  func __equal__(rhs : StringByte) -> Bool {
    return _getBool(Builtin.cmp_eq_Int8(value, rhs.value))
  }
  func __less__(rhs : StringByte) -> Bool {
    return _getBool(Builtin.cmp_ult_Int8(value, rhs.value))
  }
  func hashValue() -> Int {
    return Int(Int8(value))
  }
}

func & (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.and_Int8(lhs.value, rhs.value))
}

func | (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.or_Int8(lhs.value, rhs.value))
}

extension StringByte : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return UInt64(this).format(kind, layout)
  }
}

extension StringByte {
  constructor(v : Int8) {
    value = v.value
  }
  constructor(v : UInt8) {
    value = v.value
  }
  constructor(v : UInt32) {
    value = Builtin.trunc_Int32_Int8(v.value)
  }
}

extension Int8 {
  constructor(v : StringByte) {
    value = v.value
  }
}

extension UInt8 {
  constructor(v : StringByte) {
    value = v.value
  }
}

extension Int32 {
  constructor(v : StringByte) {
    value = Builtin.zext_Int8_Int32(v.value)
  }
}

extension UInt32 {
  constructor(v : StringByte) {
    value = Builtin.zext_Int8_Int32(v.value)
  }
}

extension UInt64 {
  constructor(v : StringByte) {
    value = Builtin.zext_Int8_Int64(v.value)
  }
}

extension Int64 {
  constructor(v : StringByte) {
    value = Builtin.zext_Int8_Int64(v.value)
  }
}

// StringByteData

var _stringByteLengthMask: Int = Int.max() >> 2
var _stringByteIsASCIIMask: Int = 1 << 64
var _stringByteIsCStringMask: Int = 1 << 63
var _stringByteHasStringBufferMask: Int = 1 << 62

typealias StringBuffer = HeapBuffer<(capacity: Int),UInt8>

extension UnsafePointer : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return Int(Builtin.ptrtoint_Int64(this.value)).format('x', "")
  }
}

struct StringByteData {

   var base : UnsafePointer<UInt8>,
   _lengthAndFlags : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> StringByteData {
     var tmp = StringByteData(UnsafePointer(base), Int(length), owner)
     tmp.setCString(false)
     return tmp
   }

   constructor(
     base: UnsafePointer<UInt8>, 
     _lengthAndFlags: Int, 
     owner: Builtin.ObjectPointer
   ) {
     this.base = base
     this._lengthAndFlags = _lengthAndFlags
     this.owner = owner
   }

   constructor() {}

   constructor(capacity: Int) {
     debugTrap(capacity >= 0)
     var buffer = StringBuffer.create(capacity, capacity)
     base = buffer.elementStorage
     owner = Builtin.castToObjectPointer(buffer)
     length = 0
     setCString(false)
     setHasStringBuffer(true)
   }

   /// \brief Make sure this StringByteData holds a unique reference
   /// to its buffer having at least newCapacity bytes of storage.
   /// Return the old owner and value of base before this call.  If
   /// the storage is reallocated, the old bytes will NOT have been
   /// copied into it.  This leaves the string in a broken state and
   /// you must repair it.
   func _makeUniqueBuffer(newCapacity: Int) 
     -> (Builtin.ObjectPointer, UnsafePointer<UInt8>
   ) {
     var isUnique = swift_isUniquelyReferenced(owner)
     var oldBase = base
     var oldOwner = owner
     var oldCapacity = capacity()

     if (oldCapacity < newCapacity || !isUnique) {
       if (newCapacity < oldCapacity * 2) {
         newCapacity = oldCapacity * 2
       }
       this = StringByteData(newCapacity)
     }
     else if hasStringBuffer() {
       // In this case we may have to push our contents backwards,
       // because our base may have been advanced past the initial
       // elements of the StringBuffer.
       var buf: StringBuffer = Builtin.castFromObjectPointer(owner)
       if (buf.elementStorage + buf.value.capacity - base < newCapacity) {
         base = buf.elementStorage
       }
     }
     return (oldOwner, oldBase)
   }

   func appendBytes(bytes: UnsafePointer<UInt8>, count: Int) {
     debugTrap(count >= 0)

     var p = this.length
     var (oldOwner, oldBase) = _makeUniqueBuffer(length + count)
     var stillASCII = isASCII()

     // If the buffer is relocated, move the old data
     if base != oldBase {
       for i in 0..p {
         base[i] = oldBase[i]
       }
     }
     for i in 0..count {
       var c = bytes[i]
       base[p + i] = c
       stillASCII = stillASCII && (c > 0x7f)
     }
     swift_keepAlive(oldOwner)

     length = p + count
     assert(p + count <= capacity())
     setASCII(stillASCII)
     setCString(false)
   }

   func capacity() -> Int {
     if !hasStringBuffer() {
       return length
     }
     var buf: StringBuffer = Builtin.castFromObjectPointer(owner)
     return buf.value.capacity
   }

   static func getNew(length : Int) -> StringByteData {
     var result = StringByteData(length)
     result.length = length
     return result
   }

   var length: Int {
   get:
      return _lengthAndFlags & _stringByteLengthMask
   set(value):
      debugTrap(value >= 0)
      debugTrap((value & _stringByteLengthMask) == value)
      _lengthAndFlags = (_lengthAndFlags & ~_stringByteLengthMask) | value
   }

   func isASCII() -> Bool {
     return (_lengthAndFlags & _stringByteIsASCIIMask) != 0
   }

   func isCString() -> Bool {
     return (_lengthAndFlags & _stringByteIsCStringMask) != 0
   }

   func hasStringBuffer() -> Bool {
     return (_lengthAndFlags & _stringByteHasStringBufferMask) != 0
   }

   func setASCII(b : Bool) {
      _lengthAndFlags = (_lengthAndFlags & ~_stringByteIsASCIIMask)
      if (b) {
        _lengthAndFlags |= _stringByteIsASCIIMask
      }
   }

   func setCString(b : Bool) {
      _lengthAndFlags = (_lengthAndFlags & ~_stringByteIsCStringMask)
      if (b) {
        _lengthAndFlags |= _stringByteIsCStringMask
      }
   }

   func setHasStringBuffer(b : Bool) {
      _lengthAndFlags = (_lengthAndFlags & ~_stringByteHasStringBufferMask)
      if (b) {
        _lengthAndFlags |= _stringByteHasStringBufferMask
      }
   }

  func getCString() -> String {
    if isCString() {
      return String(this)
    }
    var tmp : String = String(this) + Char(0)
    tmp.str_value.setCString(true)
    return tmp
  }

  func getRawCString() -> UnsafePointer<UInt8> {
    return getCString().str_value.base
  }

  subscript (i : Int) -> StringByte {
  get:
    debugTrap(UInt(i) < UInt(length))
    return StringByte(base[i])
  set(value):
    debugTrap(UInt(i) < UInt(length))
    (base + i).set(UInt8(value))
  }

  typealias EnumeratorType = StringByteData
  func getEnumeratorType() -> StringByteData { return this }

  func replPrint() {
    print('[')
    var first = true
    var total = 0
    for i in this {
      if first {
        first = false
      } else {
        print(", ")
      }
      i.replPrint()
      total = total + 1
      if total > 50 {
        print(" ...]")
        return
      }
    }
    print(']')
  }

  // Slicing via subscripting with a range.
  subscript (rng : IntEnumeratorType) -> StringByteData {
  get:
    var len = length
    debugTrap(rng.min <= len && rng.max <= len)
    return StringByteData(base + rng.min, rng.max - rng.min, owner)
  set(value):
    var len = value.length
    debugTrap(len == rng.max - rng.min)

    // Common case: the elements were updated in place, so we do not have to
    // perform any updates.
    var destStart = base + rng.min
    if value.base == destStart {
      return
    }

    // If the start of the destination slice falls inside the source slice,
    // copy backwards.
    if destStart >= value.base && destStart < value.base + len {
      var destEnd = destStart + len
      for i in value {
        --destEnd
        destEnd.set(UInt8(i))
      }

      return
    }

    // Copy the data.
    for i in value {
      destStart.set(UInt8(i))
      ++destStart
    }
  }

  func each(f : (StringByte) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : StringByte, f : (StringByte, StringByte) -> StringByte) -> StringByte {
    for i in this { val = f(val, i) }
    return val
  }

  func map(f : (StringByte) -> StringByte) -> StringByte[] {
    var len = length
    var r = new StringByte[len]
    for i in 0 .. len { r[i] = f(this[i]) }
    return r
  }
}

extension StringByteData : Enumerator {
  typealias Element = StringByte

  func isEmpty() -> Bool { return length == 0 }
  func next() -> StringByte {
    var prev = this[0]
    base = base + 1
    --length
    return prev
  }
}

extension StringByteData : Equatable {
  func __equal__(rhs: StringByteData) -> Bool {
    return base == rhs.base && length == rhs.length
  }
}
