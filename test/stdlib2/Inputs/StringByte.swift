struct StringByte : Comparable {
  var value : Builtin.Int8

  static func convertFromIntegerLiteral(val : Builtin.Int8) -> StringByte {
    return StringByte(val)
  }

  func replPrint() {
    print(UInt64(this))
  }
  // FIXME:
  // static var max : StringByte { get { return 0xFF } }
  // static var min : StringByte { get { return 0 } }
  static func max() -> StringByte { return 0xFF }
  static func min() -> StringByte { return 0 }
}

func [infix_left=190] + (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.add_Int8(lhs.value, rhs.value))
}

func [infix_left=190] - (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.sub_Int8(lhs.value, rhs.value))
}

func [infix=160] == (lhs : StringByte, rhs : StringByte) -> Bool {
  return _getBool(Builtin.cmp_eq_Int8(lhs.value, rhs.value))
}

func [infix=160] != (lhs : StringByte, rhs : StringByte) -> Bool {
  return !(lhs == rhs)
}

func [infix=170] < (lhs : StringByte, rhs : StringByte) -> Bool {
  return _getBool(Builtin.cmp_ult_Int8(lhs.value, rhs.value))
}

func [infix=170] > (lhs : StringByte, rhs : StringByte) -> Bool {
  return rhs < lhs
}

func [infix=170] <= (lhs : StringByte, rhs : StringByte) -> Bool {
  return !(rhs < lhs)
}

func [infix=170] >= (lhs : StringByte, rhs : StringByte) -> Bool {
  return !(lhs < rhs)
}

func [infix_left=150] & (lhs: StringByte, rhs: StringByte) -> StringByte {
  return StringByte(Builtin.and_Int8(lhs.value, rhs.value))
}

func [infix_left=130] | (lhs: StringByte, rhs: StringByte) -> StringByte {
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

struct StringByteData : Enumerable {
   var base : UnsafePointer<UInt8>,
   length : Int,
   owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> StringByteData {
     typealias UnsafePtr = UnsafePointer<UInt8>
     return StringByteData(UnsafePtr(base), Int(length), owner)
   }

   static func getNew(length : Int) -> StringByteData {
     var temp = new StringByte[length]
     return convertFromHeapArray(temp.base.value, temp.owner, temp.length.value)
   }

   func getLength() -> Int {
      return length & (Int.max() >> 1)
   }

   func setLength(len : Int) {
      length &= Int.min()
      length |= (len & Int.max())
   }

   func isASCII() -> Bool {
     return (length & Int64.min()) == 0
   }

   func isCString() -> Bool {
     return (length & (1 << 62)) == 0
   }

   func setASCII(b : Bool) {
     if b {
       length &= Int64.max()
     } else {
       length |= Int64.min()
     }
   }

   func setCString(b : Bool) {
     if b {
       length &= ~(1 << 62)
     } else {
       length |= (1 << 62)
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
    get {
      debugTrap(UInt(i) < UInt(getLength()))
      return StringByte((base + i).get())
    }

    set {
      debugTrap(UInt(i) < UInt(getLength()))
      (base + i).set(UInt8(value))
    }
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
    get {
      var len = getLength()
      debugTrap(rng.min <= len && rng.max <= len)
      return StringByteData(base + rng.min, rng.max - rng.min, owner)
    }

    set {
      var len = value.getLength()
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
  }

  func each(f : (StringByte) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : StringByte, f : (StringByte, StringByte) -> StringByte) -> StringByte {
    for i in this { val = f(val, i) }
    return val
  }

  func map(f : (StringByte) -> StringByte) -> StringByte[] {
    var len = getLength()
    var r = new StringByte[len]
    for i in 0 .. len { r[i] = f(this[i]) }
    return r
  }
}

extension StringByteData : Enumerator {
  typealias Element = StringByte

  func isEmpty() -> Bool { return getLength() == 0 }
  func next() -> StringByte {
    var prev = this[0]
    base = base + 1
    setLength(getLength() - 1)
    return prev
  }
}
