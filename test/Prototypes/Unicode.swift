// RUN: %target-build-swift -I %S/icu %s -o %t
// RUN: %target-run %t
// REQUIRES: executable_test

import StdlibUnittest
import ICU

/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
public struct ParsedUnicode<
  CodeUnits: BidirectionalCollection,
  Encoding: UnicodeEncoding
>
where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : BidirectionalCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
  let codeUnits: CodeUnits
  
  init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
    self.codeUnits = codeUnits
  }
}

extension ParsedUnicode {
  // Because parsing produces a buffer and a new index, to avoid
  // repeatedly decoding the same data, this index stores that buffer
  // and the next index.  This would obviously be more complicated if
  // the buffer contained more than a single scalar (and it probably
  // should).
  public struct Index : Comparable {
    let base: CodeUnits.Index
    // FIXME: We might get a much better memory footprint if we used a
    // UInt8 to store the distance between base and next, rather than
    // storing next explicitly.  CodeUnits will be random-access in
    // practice.
    let next: CodeUnits.Index
    // FIXME: there should be an invalid inhabitant we can use in
    // EncodedScalar so as not to waste a separate bool here.
    let scalar: Encoding.EncodedScalar?

    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs.base < rhs.base
    }
    public static func == (lhs: Index, rhs: Index) -> Bool {
      return lhs.base == rhs.base
    }
  }
}

/// Collection Conformance
extension ParsedUnicode : BidirectionalCollection {
  public var startIndex: Index {
    if _slowPath(codeUnits.isEmpty) { return endIndex }
    let s = codeUnits.startIndex
    return index(after: Index(base: s, next: s, scalar: nil))
  }
  
  public var endIndex: Index {
    let s = codeUnits.endIndex
    return Index(base: s, next: s, scalar: nil)
  }
  
  public subscript(i: Index) -> Encoding.EncodedScalar {
    if let r = i.scalar {
      return r
    }
    return index(after:
      Index(base: i.base, next: i.base, scalar: nil)).scalar!
  }

  public func index(after i: Index) -> Index {
    var remainder = codeUnits[i.next..<codeUnits.endIndex]
    while true {
      switch Encoding.parse1Forward(remainder, knownCount: 0) {
      case .valid(let scalar, let nextIndex):
        return Index(base:i.next, next: nextIndex, scalar: scalar)
      case .error(let nextIndex):
        // FIXME: don't go through UnicodeScalar once this is in the stdlib
        if let replacement = Encoding.encode(
          UTF32.EncodedScalar(UnicodeScalar(0xFFFD)!)) {
          return Index(
            base:i.next, next: nextIndex,
            scalar: replacement)
        }
        remainder = remainder.dropFirst()
      case .emptyInput:
        return endIndex
      }
    }
  }

  public func index(before i: Index) -> Index {
    var remainder = codeUnits[codeUnits.startIndex..<i.base]
    while true {
      switch Encoding.parse1Reverse(remainder, knownCount: 0) {
      case .valid(let scalar, let priorIndex):
        return Index(base: priorIndex, next: i.base, scalar: scalar)
      case .error(let priorIndex):
        // FIXME: don't go through UnicodeScalar once this is in the stdlib
        if let replacement = Encoding.encode(
          UTF32.EncodedScalar(UnicodeScalar(0xFFFD)!)) {
          return Index(
            base: priorIndex, next: i.base, 
            scalar: replacement)        
        }
        remainder = remainder.dropLast()
      case .emptyInput:
        fatalError("Indexing past start of code units")
      }
    }
  }
}

/// Given `CodeUnits` representing text that has been encoded with
/// `FromEncoding`, provides a collection of `ToEncoding.CodeUnit`s
/// representing the same text.
struct TranscodedView<
  CodeUnits : BidirectionalCollection,
  FromEncoding : UnicodeEncoding,
  ToEncoding : UnicodeEncoding
> 
where FromEncoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : BidirectionalCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element
{
  // We could just be a generic typealias as this type, but it turns
  // out to be impossible, or nearly so, to write the init() below.
  // Instead, we wrap an instance of Base.
  typealias Base = FlattenBidirectionalCollection<
    LazyMapBidirectionalCollection<
      ParsedUnicode<CodeUnits, FromEncoding>,
      ToEncoding.EncodedScalar
    >
  >
  let base: Base
}

extension TranscodedView : BidirectionalCollection {
  public var startIndex : Base.Index {
    return base.startIndex
  }
  public var endIndex : Base.Index {
    return base.endIndex
  }
  public subscript(i: Base.Index) -> Base.Iterator.Element {
    return base[i]
  }
  public init(_ codeUnits: CodeUnits,
    from src: FromEncoding.Type = FromEncoding.self,
    to dst: ToEncoding.Type = ToEncoding.self
  ) {
    base = Base(ParsedUnicode(codeUnits, src).lazy.map {
        dst.encode($0)!
      })
  }
  public func index(after i: Base.Index) -> Base.Index {
    return base.index(after: i)
  }
  public func index(before i: Base.Index) -> Base.Index {
    return base.index(before: i)
  }
}

protocol UnicodeStorage {
  associatedtype Encoding: UnicodeEncoding
  associatedtype CodeUnits: RandomAccessCollection
  /* where CodeUnits.Iterator.Element == Encoding.CodeUnit */
  var codeUnits: CodeUnits {get}
  
  associatedtype ValidUTF8View : BidirectionalCollection
  // where ValidUTF8View.Iterator.Element == UTF8.CodeUnit */
  // = TranscodedView<CodeUnits, Encoding, UTF8>
  var utf8: ValidUTF8View {get}
  
  associatedtype ValidUTF16View : BidirectionalCollection
  // where ValidUTF16View.Iterator.Element == UTF16.CodeUnit
  // = TranscodedView<CodeUnits, Encoding, UTF16>
  var utf16: ValidUTF16View {get}
  
  associatedtype ValidUTF32View : BidirectionalCollection
  // where ValidUTF32View.Iterator.Element == UTF32.CodeUnit
  // = TranscodedView<CodeUnits, Encoding, UTF32>
  var utf32: ValidUTF32View {get}
  
  associatedtype ExtendedASCII : BidirectionalCollection // FIXME: Can this be Random Access?
  /* where ExtendedASCII.Iterator.Element == UInt32 */
  var extendedASCII: ExtendedASCII {get}

  associatedtype Characters : BidirectionalCollection
  /* where Characters.Iterator.Element == Character */
  var characters: Characters { get }
  
  func isASCII(scan: Bool/* = true */) -> Bool 
  func isLatin1(scan: Bool/* = true */) -> Bool 
  func isNormalizedNFC(scan: Bool/* = true*/) -> Bool
  func isNormalizedNFD(scan: Bool/* = true*/) -> Bool
  func isInFastCOrDForm(scan: Bool/* = true*/) -> Bool
}

extension UnicodeStorage {
  func withUText<R>(_ body: (UnsafePointer<UText>)->R) -> R {
    var vtable = UTextFuncs(
      tableSize: Int32(MemoryLayout<UTextFuncs>.stride),
      reserved1: 0,
      reserved2: 0,
      reserved3: 0,

      // UText* UTextClone(
      //   UText *dest, const UText *src, UBool deep, UErrorCode *status)
      //
      // clone a UText. Much like opening a UText where the source text is
      // itself another UText.
      //
      // A deep clone will copy both the UText data structures and the
      // underlying text. The original and cloned UText will operate completely
      // independently; modifications made to the text in one will not effect
      // the other. Text providers are not required to support deep clones. The
      // user of clone() must check the status return and be prepared to handle
      // failures.
      //
      // A shallow clone replicates only the UText data structures; it does not
      // make a copy of the underlying text. Shallow clones can be used as an
      // efficient way to have multiple iterators active in a single text string
      // that is not being modified.
      //
      // A shallow clone operation must not fail except for truly exceptional
      // conditions such as memory allocation failures.
      //
      // A UText and its clone may be safely concurrently accessed by separate
      // threads. This is true for both shallow and deep clones. It is the
      // responsibility of the Text Provider to ensure that this thread safety
      // constraint is met.
      
      clone: nil, //

      // int64_t UTextNativeLength(UText *ut)
      //
      // the length, in the native units of the original text string.
      nativeLength: { return numericCast(codeUnits.count) }, //

      // UBool UTextAccess(UText *ut, int64_t nativeIndex, UBool forward)
      //
      // Get the description of the text chunk containing the text at a
      // requested native index. The UText's iteration position will be left at
      // the requested index. If the index is out of bounds, the iteration
      // position will be left at the start or end of the string, as
      // appropriate.
      //
      // Chunks must begin and end on code point boundaries. A single code point
      // comprised of multiple storage units must never span a chunk boundary.
      access: nil, //

      // typedef int32_t UTextExtract(UText *ut, int64_t nativeStart, int64_t
      // nativeLimit, UChar *dest, int32_t destCapacity, UErrorCode *status)
      // 
      // Extract text from a UText into a UChar buffer. The range of text to be
      // extracted is specified in the native indices of the UText
      // provider. These may not necessarily be UTF-16 indices.
      // 
      // The size (number of 16 bit UChars) in the data to be extracted is
      // returned. The full amount is returned, even when the specified buffer
      // size is smaller.
      // 
      // The extracted string will (if you are a user) / must (if you are a text
      // provider) be NUL-terminated if there is sufficient space in the
      // destination buffer.

      extract: nil, //

      
      replace: nil,
      copy: nil,
      
      // int64_t UTextMapOffsetToNative(const UText *ut)
      // 
      // Map from the current UChar offset within the current text chunk to the
      // corresponding native index in the original source text.
      // 
      // This is required only for text providers that do not use native UTF-16
      // indexes.
      mapOffsetToNative: nil,


      // int32_t UTextMapNativeIndexToUTF16(const UText *ut, int64_t nativeIndex)
      // Function type declaration for UText.mapIndexToUTF16().
      // 
      // Map from a native index to a UChar offset within a text chunk. Behavior
      // is undefined if the native index does not fall within the current
      // chunk.
      // 
      // This function is required only for text providers that do not use
      // native UTF-16 indexes.
      mapNativeIndexToUTF16: nil,
      
      close: nil,
      spare1: nil,
      spare2: nil,
      spare3: nil)

    func impl(
      _ vtable: UnsafePointer<UTextFuncs>,
      _ body: (UnsafePointer<UText>)->R // why must I pass body explicitly here?
    ) -> R {                            // if I don't, the compiler complains it
                                        // might escape.
      var u = UText(
        magic: UInt32(UTEXT_MAGIC),
        flags: 0,
        providerProperties: 0,
        sizeOfStruct: Int32(MemoryLayout<UText>.stride),
        chunkNativeLimit: 0,
        extraSize: 0,
        nativeIndexingLimit: 0,
        chunkNativeStart: 0,
        chunkOffset: 0,
        chunkLength: 0,
        chunkContents: nil,
        pFuncs: vtable,
        pExtra: nil,
        context: nil,
        p: nil, q: nil, r: nil,
        privP: nil,
        a: 0, b: 0, c: 0,
        privA: 0, privB: 0, privC: 0)
      return body(&u)
    }
    return impl(&vtable, body)
  }
}

struct Latin1Storage<Base : RandomAccessCollection> : UnicodeStorage
where Base.Iterator.Element == UInt8, Base.Index == Base.SubSequence.Index,
Base.SubSequence.SubSequence == Base.SubSequence,
Base.Iterator.Element == UInt8,
Base.SubSequence.Iterator.Element == Base.Iterator.Element
{
  typealias Encoding = Latin1
  typealias CodeUnits = Base
  let codeUnits: CodeUnits
  let _isASCII: Bool?

  init(_ codeUnits: CodeUnits, isASCII: Bool? = nil) {
    self.codeUnits = codeUnits
    self._isASCII = isASCII
  }
  
  typealias ValidUTF8View = TranscodedView<CodeUnits, Encoding, UTF8>
  var utf8: ValidUTF8View { return ValidUTF8View(codeUnits) }
  
  typealias ValidUTF16View = TranscodedView<CodeUnits, Encoding, UTF16>
  var utf16: ValidUTF16View { return ValidUTF16View(codeUnits) }
  
  typealias ValidUTF32View = TranscodedView<CodeUnits, Encoding, UTF32>
  var utf32: ValidUTF32View { return ValidUTF32View(codeUnits) }
  
  typealias ExtendedASCII = LazyMapRandomAccessCollection<CodeUnits, UInt32>
  var extendedASCII: ExtendedASCII {
    return codeUnits.lazy.map { UInt32($0) }
  }

  typealias Characters = LazyMapRandomAccessCollection<CodeUnits, Character>
  var characters: Characters {
    return codeUnits.lazy.map {
      Character(UnicodeScalar(UInt32($0))!)
    }
  }
  
  func isASCII(scan: Bool = true) -> Bool {
    if let result = _isASCII { return result }
    return scan && !codeUnits.contains { $0 > 0x7f }
  }
  func isLatin1(scan: Bool = true) -> Bool {
    return true
  }
  func isNormalizedNFC(scan: Bool = true) -> Bool {
    return true
  }
  func isNormalizedNFD(scan: Bool = true) -> Bool {
    return true
  }
  func isInFastCOrDForm(scan: Bool = true) -> Bool {
    return true
  }
}


/*
print(s32.count, s16to32.count)
for (n, (x, y)) in zip(s32, s16to32).enumerated() {
  if x != y {
    print("at: \(n), \(UnicodeScalar(x)!) (\(x)) != \(UnicodeScalar(y)!) (\(y))")
  }
}


enum UnicodeContentWidth: UInt32 {
case ASCII = 1 << 7
case Latin1 = 1 << 8
case UCS2 = 1 << 16
case Unlimited = 1 << 21
}

*/

var t = TestSuite("t")
t.test("basic") {
  let s = "abcdefghijklmnopqrstuvwxyz\n"
  + "Σὲ 👥🥓γνωρίζω ἀπὸ τὴν κόψη χαῖρε, ὦ χαῖρε, ᾿Ελευθεριά!\n"
  + "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι,\n"
  + "გთხოვთ ახლავე გაიაროთ რეგისტრაცია Unicode-ის მეათე საერთაშორისო\n"
  + "Зарегистрируйтесь сейчас на Десятую Международную Конференцию по\n"
  + "  ๏ แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช  พระปกเกศกองบู๊กู้ขึ้นใหม่\n"
  + "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"
  let s32 = s.unicodeScalars.lazy.map { $0.value }
  let s16 = s.utf16
  let s8 = Array(s.utf8)
  let s16to32 = TranscodedView(s16, from: UTF16.self, to: UTF32.self)
  let s16to8 = TranscodedView(s16, from: UTF16.self, to: UTF8.self)
  let s8to16 = TranscodedView(s8, from: UTF8.self, to: UTF16.self)
  let s8Vto16 = TranscodedView(s8, from: ValidUTF8.self, to: UTF16.self)
  expectTrue(s32.elementsEqual(s16to32))
  expectTrue(s8.elementsEqual(s16to8))
  expectTrue(s16.elementsEqual(s8to16))
  expectTrue(s16.elementsEqual(s8Vto16))

  expectTrue(s32.reversed().elementsEqual(s16to32.reversed()))
  expectTrue(s8.reversed().elementsEqual(s16to8.reversed()))
  expectTrue(s16.reversed().elementsEqual(s8to16.reversed()))
  expectTrue(s16.reversed().elementsEqual(s8Vto16.reversed()))

  do {
    // We happen to know that alphabet is non-ASCII, but we're not going to say
    // anything about that.
    let alphabet = Latin1Storage(s8.prefix(27))
    expectTrue(alphabet.isASCII())
    expectFalse(alphabet.isASCII(scan: false))
    
    // We know that if you interpret s8 as Latin1, it has a lot of non-ASCII
    let nonASCII = Latin1Storage(s8) 
    expectFalse(nonASCII.isASCII(scan: true))
    expectFalse(nonASCII.isASCII(scan: false))
  }

  do {
    let alphabet = Latin1Storage(s8.prefix(27), isASCII: true)
    let nonASCII = Latin1Storage(s8, isASCII: false)
    expectTrue(alphabet.isASCII())
    expectTrue(alphabet.isASCII(scan: false))
    expectFalse(nonASCII.isASCII(scan: true))
    expectFalse(nonASCII.isASCII(scan: false))
  }
}
runAllTests()
