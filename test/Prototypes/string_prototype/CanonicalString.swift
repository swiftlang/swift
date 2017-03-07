import Swift

// The preferred format for non-Latin1-able strings in Swift.
// In-memory UTF16 encoding in TODO-normal-form.
struct SwiftCanonicalString {
  typealias Element = UInt16
  typealias CodeUnits = _StringBuffer<Element>
  typealias Encoding = UTF16
  typealias Storage = UnicodeStorage<CodeUnits, Encoding>
  var _storage: Storage

  // Always set at construction time, conservatively updated at modification
  var isKnownASCII: Bool {
    get { return _storage.codeUnits._storage.isKnownASCII }
    set { _storage.codeUnits._storage.isKnownASCII = newValue }
  }
  var isKnownLatin1: Bool {
    get { return _storage.codeUnits._storage.isKnownLatin1 }
    set { _storage.codeUnits._storage.isKnownLatin1 = newValue }
  }

  // Perform a copy, transcoding, and normalization of the supplied code units
  init<OtherCodeUnits, OtherEncoding>(
    _ other: UnicodeStorage<OtherCodeUnits, OtherEncoding>
  ) {
    // FIXME: do normalization on the fly, perhaps a normalized view?
    let otherUTF16 = other.transcoded(to: Encoding.self)

    // TODO: more effient to allocate too much space (guessed by encoding
    // sizes), and copy in, rather than linear time count operation
    let newCount = otherUTF16.count

    let newStringStorage = _StringStorage<UInt16>(
      count: newCount, minimumCapacity: newCount
    )

    self._storage = UnicodeStorage(
      CodeUnits(newStringStorage),
      Encoding.self
    )
    
    // Start off as true, we will unset when we find a violation
    self.isKnownASCII = true
    self.isKnownLatin1 = true

    // Copy in
    // FIXME: why can't I use .indices below?
    for (idx, elt) in zip(0..<newCount, otherUTF16) {
      if (elt > 0xff) {
        self.isKnownLatin1 = false
        self.isKnownASCII = false
      } else if (elt > 0x7f) {
        isKnownASCII = false
      }
      newStringStorage[idx] = elt
    }
  }

  init() {
    self._storage = UnicodeStorage(CodeUnits(), Encoding.self)
    // Empty strings have all the encodings!
    self.isKnownASCII = true
    self.isKnownLatin1 = true
  }

  init(uninitializedCount count: Int) {
    self._storage = UnicodeStorage(
      CodeUnits(_uninitializedCount: count, minimumCapacity: count), 
      Encoding.self)
    self.isKnownASCII = true
    self.isKnownLatin1 = true
  }
}

extension SwiftCanonicalString {
  init<
    OtherCodeUnits: RandomAccessCollection,
    OtherEncoding: UnicodeEncoding
  >
  (
    codeUnits: OtherCodeUnits, encodedWith otherEncoding: OtherEncoding.Type
  )
  where
    OtherEncoding.EncodedScalar.Iterator.Element == OtherCodeUnits.Iterator.Element,
    OtherCodeUnits.SubSequence : RandomAccessCollection,
    OtherCodeUnits.SubSequence.Index == OtherCodeUnits.Index,
    OtherCodeUnits.SubSequence.SubSequence == OtherCodeUnits.SubSequence,
    OtherCodeUnits.SubSequence.Iterator.Element == OtherCodeUnits.Iterator.Element
  {
    self.init(UnicodeStorage(codeUnits, otherEncoding))
  }
}

extension SwiftCanonicalString : Unicode {
  var codeUnits: CodeUnits { return _storage.codeUnits }

  typealias ValidUTF8View = Storage.TranscodedView<UTF8>
  var utf8: ValidUTF8View { return ValidUTF8View(codeUnits) }

  typealias ValidUTF16View = Storage.TranscodedView<UTF16>
  var utf16: ValidUTF16View { return ValidUTF16View(codeUnits) }

  typealias ExtendedASCII = LazyMapRandomAccessCollection<CodeUnits, UInt32>
  var extendedASCII: ExtendedASCII {
    return codeUnits.lazy.map { UInt32($0) }
  }

  typealias ValidUTF32View = Storage.TranscodedView<UTF32>
  var utf32: ValidUTF32View { return ValidUTF32View(codeUnits) }

  // typealias EncodedScalars = Storage.EncodedScalars
  // var encodedScalars: EncodedScalars {
  //   return _storage.scalars
  // }

  typealias Characters = Storage.CharacterView
  var characters: Characters {
    return _storage.characters
  }

  func isASCII(scan: Bool = true) -> Bool {
    if isKnownASCII {
      return true
    }
    return scan && false // TODO: perform scan?
  }
  func isLatin1(scan: Bool = true) -> Bool {
    if isKnownLatin1 {
      return true
    }
    return scan && false // TODO: perform scan?
  }
  func isNormalizedNFC(scan: Bool = true) -> Bool {
    // TODO: is this the ideal normal form for us?
    return true
  }
  func isNormalizedNFD(scan: Bool = true) -> Bool {
    return false
    // TODO: perform scan perhaps? If every scalar is a whole grapheme, then
    // this would be true
  }
  func isInFastCOrDForm(scan: Bool = true) -> Bool {
    // FIXME: *almost* all NFC is FCC, but not all ...
    return true
  }
}

// Super dumb comparable conformance...
extension SwiftCanonicalString : Comparable {
  static func ==(
    _ lhs: SwiftCanonicalString, rhs: SwiftCanonicalString
  ) -> Bool {
    return lhs._storage.ordered(with: rhs._storage) == .same
  }
  static func <(
    _ lhs: SwiftCanonicalString, rhs: SwiftCanonicalString
  ) -> Bool {
    return lhs._storage.ordered(with: rhs._storage) == .before
  }
}

extension SwiftCanonicalString : Hashable {
  var hashValue : Int {
    var hasher = _SipHash13Context(key: _Hashing.secretKey)
    for x in _storage.nfcNormalizedUTF16 { hasher.append(x) }
    let resultBits = hasher.finalizeAndReturnHash()
#if arch(i386) || arch(arm)
    return Int(truncatingBitPattern: resultBits)
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
    return Int(Int64(bitPattern: resultBits))
#endif
  }
}

extension SwiftCanonicalString /* : RangeReplaceableCollection */ {
  mutating func replaceSubrange<C: Collection>(
    _ subrange: Range<Int>, with newValues: C
  )
  where C.Iterator.Element == Element
  {
    _precondition(subrange.lowerBound >= codeUnits.startIndex,
      "String replace: subrange start is negative")
    _precondition(subrange.upperBound <= codeUnits.endIndex,
      "String replace: subrange extends past the end")

    // TODO: update these properly (this is currently correct but bad)
    isKnownASCII = false
    isKnownLatin1 = false

    _storage.codeUnits.replaceSubrange(subrange, with: newValues)
  }
}
