// FIXME: Char should be renamed CodePoint in the compiler
typealias CodePoint = Char

struct IsSpace : Predicate {
  typealias Arguments = CodePoint
  typealias Result = Bool
  func __call__(c: CodePoint) -> Result {
    return c.isSpace()
  }
}

struct IsEqualTo<T requires T: Equatable> : Predicate {
  var target: GenericIVar<T>

  constructor(target: T) {
    this.target.value = target
  }

  typealias Arguments = T
  typealias Result = Bool

  func __call__(x: T) -> Result {
    return x == target.value
  }
}

struct CodePointIndex : BidirectionalIndex, Comparable {
  var data : StringByteData
  var position : Int

  func __equal__(rhs: CodePointIndex) -> Bool {
    assert(data == rhs.data, "Can't compare Indices from different Strings")
    return position == rhs.position
  }
  func __less__(rhs: CodePointIndex) -> Bool {
    assert(data == rhs.data, "Can't compare Indices from different Strings")
    return position < rhs.position
  }

  func succ() -> CodePointIndex {
    assert(position < data.length)
    var pos = position
    var c0 = (data.base + pos).get()
    ++pos
    if c0 >= 0x80 {
      ++pos
      if c0 >= 0xE0 {
        ++pos
        if c0 >= 0xF0 { 
          ++pos
        }
      }
    }
    return CodePointIndex(data, pos)
  }

  func pred() -> CodePointIndex {
    var pos = position
    var p = data.base
    assert(pos > 0)

    if (((p + --pos).get() & 0xC0) == 0x80) {
      if (((p + --pos).get() & 0xC0) == 0x80) {
        if (((p + --pos).get() & 0xC0) == 0x80) { 
          --pos
        }
      }
    }
    return CodePointIndex(data, pos)
  }
}

/// \brief Represents the sequence of Unicode code points in a
/// UTF8-encoded StringByteData object
struct CodePoints
{
  constructor(s: String) {
    this.str_value = s.str_value
  }

  func [conversion] __conversion() -> String {
    return String(str_value)
  }
  
  func isEmpty() -> Bool { return str_value.length == 0 }

  func first() -> Char {
    var p = str_value.base

    // one octet (7 bits)
    var c0 : UInt8 = p.get()

    if c0 < 0x80 {
      return Char(UInt32(c0))
    }
    
    var c1 = (++p).get()

    // start with octet 1 (we'll mask off high bits later)
    var result = UInt32(c0)
    result = (result << 6) | UInt32(c1 & 0x3F)  // merge octet 2
    if c0 < 0xE0 {
      return CodePoint(result & 0x000007FF)     // 11 bits
    }
    c1 = (++p).get()                            // prefetch octet 3
    result = (result << 6) | UInt32(c1 & 0x3F)  // merge octet 3
    if c0 < 0xF0 {
      return CodePoint(result & 0x0000FFFF)     // 16 bits
    }
    c1 = (++p).get()                            // prefetch octet 4
    result = (result << 6) | UInt32(c1 & 0x3F)  // merge octet 4
    return CodePoint(result & 0x001FFFFF)       // 21 bits
  }

  func last() -> Char {
    var p = str_value.base + str_value.length
    var c = (--p).get()

    if c < 0x80 {
      return Char(UInt32(c))
    }

    var result = UInt32(c & 0x3F)

    c = (--p).get()
    var more = ~c & 0x40
    var mask = (more >> 1) | 0x1F
    result |= UInt32(c & mask) << 6
    if more == 0 { return Char(result) }

    c = (--p).get()
    more = ~c & 0x40
    mask = (more >> 2) | 0x0F
    result |= UInt32(c & mask) << (6+6)
    if more == 0 { return Char(result) }

    c = (--p).get()
    more = ~c & 0x40
    mask = (more >> 3) | 0x07
    result |= UInt32(c & mask) << (6+6+6)
    return Char(result)
  }

  func startsWith(prefix: CodePoints) -> Bool {
    return swift.startsWith(this.getEnumeratorType(), prefix.getEnumeratorType())
  }

  func endsWith(suffix: CodePoints) -> Bool {
    return swift.startsWith(
      reverse(this).getEnumeratorType(), 
      reverse(suffix).getEnumeratorType())
  }

  /// \brief Return a sequence of consecutive whitespace-separated
  /// substrings of the string S.  If maxsplit is given, at most
  /// maxsplit splits are done. Any whitespace string is a separator
  /// and empty strings are removed from the result.
  func split(maxSplit: Int = Int.max())
  -> CodePoints[]
  {
    return swift.split(this, IsSpace(), maxSplit)
  }

  /// \brief Return the sequence of consecutive (possibly-empty)
  /// substrings that do not contain separator.  If maxsplit is given,
  /// at most maxsplit splits are done. The result may contain empty
  /// strings.
  func split(separator: Char, maxSplit: Int = Int.max())
  -> CodePoints[]
  {
    return swift.split(this, IsEqualTo(separator), maxSplit, allowEmptySlices: true)
  }

  var str_value : StringByteData
}

extension CodePoints : Equatable {
  func __equal__(rhs: CodePoints) -> Bool {
    return String(str_value) == String(rhs.str_value)
  }
}

extension CodePoints : Indexable {
  typealias Element = CodePoint
  typealias IndexType = CodePointIndex

  func begin() -> IndexType {
    return IndexType(str_value, 0)
  }
  func end() -> IndexType {
    return IndexType(str_value, str_value.length)
  }

  func __getitem__(i: IndexType) -> Element {
    assert(str_value == i.data, "Attempting to index a String using an index from another String")
    var p = str_value.base + i.position

    var c0 : UInt8 = p.get()

    if c0 < 0x80 {
      return Char(UInt32(c0))                   // one octet (7 bits)
    }
    
    // start with octet 1 (we'll mask off high bits later)
    var c1 = (++p).get()

    var result = UInt32(c0)
    result = (result << 6) | UInt32(c1 & 0x3F)
    if c0 < 0xE0 {
      return CodePoint(result & 0x000007FF)     // two octets (11 bits)
    }
    c1 = (++p).get()                      
    result = (result << 6) | UInt32(c1 & 0x3F)
    if c0 < 0xF0 {
      return CodePoint(result & 0x0000FFFF)      // three octets (16 bits)
    }
    c1 = (++p).get()
    result = (result << 6) | UInt32(c1 & 0x3F)
    return CodePoint(result & 0x001FFFFF)        // four octets (21 bits)
  }

  subscript(i: IndexType) -> Element {
  get:
    return __getitem__(i)
  }
}

extension CodePoints : Sliceable {
  func __slice__(start: IndexType, finish: IndexType) -> CodePoints {
    assert(
      str_value == start.data, 
      "Attempting to slice a string using a start index from another String")
    assert(
      str_value == finish.data, 
      "Attempting to slice a string using an end index from another String")

    // TODO: Decide what should be an error.  For now I think it's
    // better to return an empty String when finish < start
    var ret = CodePoints("")
    if finish > start { 
      ret.str_value = str_value[start.position..finish.position]
    }
    return ret
  }

  subscript(r: Range<IndexType>) -> CodePoints {
  get:
    return __slice__(r.begin(), r.end())
  }
}

extension CodePoints : Enumerable {
  // FIXME: This is not the most efficient way to implement
  // Enumerators for CodePoints, since:
  // a. they will essentially contain two copies of the same
  //    StringByteData (one for the IndexType and one for the Indexable).
  // b. enumeration will have to repeat much of the work of decoding
  //    characters when moving forward.  
  // At some point we'll want to refactor for efficiency, but probably
  // not before Generator becomes the official iteration protocol.
  typealias EnumeratorType = IndexableEnumerator<CodePoints, Range<IndexType> >
  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(this, indices(this))
  }
}

extension CodePoints : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return String(this.str_value).format(kind, layout)
  }
}

func print(x: CodePoints) { print(String(x)) }
func println(x: CodePoints) { println(String(x)) }
