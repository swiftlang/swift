import Swift


// A Latin1-only String
// TODO: Investigate if this can be comprised entirely of typealias
// and data stored in _StringBuffer
struct Latin1String {
  typealias Element = UInt8
  typealias CodeUnits = _StringBuffer<_Latin1StringStorage>
  typealias Encoding = Latin1
  typealias Storage = UnicodeStorage<CodeUnits, Encoding>
  var _storage: Storage

  // Store some bits TODO: should be packed into our storage ref?
  //
  // Always set at construction time, conservatively updated at modification
  var isKnownASCII: Bool { return false }
  var isKnownLatin1: Bool { return true }

  // Perform a copy, transcoding, and normalization of the supplied code units
  init<OtherCodeUnits, OtherEncoding>(
    _ other: UnicodeStorage<OtherCodeUnits, OtherEncoding>
  ) {
    let otherLatin1 = other.transcoded(to: Encoding.self)
    let newCount = otherLatin1.count
    let newStringStorage = _Latin1StringStorage(
      _uninitializedCount: numericCast(newCount))

    // Copy in
    // FIXME: why can't I use .indices below?
    for (idx, elt) in otherLatin1.enumerated() {
      newStringStorage[idx] = elt
    }

    _storage = UnicodeStorage(
      CodeUnits(newStringStorage),
      Encoding.self
    )
  }

  init() {
    _storage = UnicodeStorage(CodeUnits(), Encoding.self)
  }

  init(uninitializedCount count: Int) {
    _storage = UnicodeStorage(
      CodeUnits(_uninitializedCount: count, minimumCapacity: count), 
      Encoding.self)
  }
}

extension Latin1String {
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

extension Latin1String : Unicode {
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
    return scan 
      ? !codeUnits.contains { $0 > 0x7f } 
      : false // TODO: store this fact in _StringBuffer
  }
  func isLatin1(scan: Bool = true) -> Bool {
    return true
  }
  func isNormalizedNFC(scan: Bool = true) -> Bool {
    return true
  }
  func isNormalizedNFD(scan: Bool = true) -> Bool {
    fatalError("TODO")
  }
  func isInFastCOrDForm(scan: Bool = true) -> Bool {
    // FIXME: *almost* all NFC is FCC, but not all ...
    fatalError("TODO")
  }
}

// Super dumb comparable conformance...
extension Latin1String : Comparable {
  static func ==(
    _ lhs: Latin1String, rhs: Latin1String
  ) -> Bool {
    return lhs._storage.ordered(with: rhs._storage) == .same
  }
  static func <(
    _ lhs: Latin1String, rhs: Latin1String
  ) -> Bool {
    return lhs._storage.ordered(with: rhs._storage) == .before
  }
}

extension Latin1String /* : RangeReplaceableCollection */ {
  mutating func replaceSubrange<C: Collection>(
    _ subrange: Range<Int>, with newValues: C
  )
  where C.Iterator.Element == Element
  {
    _precondition(subrange.lowerBound >= codeUnits.startIndex,
      "String replace: subrange start is negative")
    _precondition(subrange.upperBound <= codeUnits.endIndex,
      "String replace: subrange extends past the end")

    _storage.codeUnits.replaceSubrange(subrange, with: newValues)
  }
}

