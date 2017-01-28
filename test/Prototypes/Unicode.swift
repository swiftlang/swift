// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

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

//===--- Quickie Tests ----------------------------------------------------===//
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
assert(s32.elementsEqual(s16to32))
assert(s8.elementsEqual(s16to8))
assert(s16.elementsEqual(s8to16))
assert(s16.elementsEqual(s8Vto16))

assert(s32.reversed().elementsEqual(s16to32.reversed()))
assert(s8.reversed().elementsEqual(s16to8.reversed()))
assert(s16.reversed().elementsEqual(s8to16.reversed()))
assert(s16.reversed().elementsEqual(s8Vto16.reversed()))
//===----------------------------------------------------------------------===//

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
  
  func contentWidth(knownWithoutScanning: Bool/* = false*/) -> UInt32
  func isNormalizedNFC(knownWithoutScanning: Bool/* = false*/) -> Bool
  func isNormalizedNFD(knownWithoutScanning: Bool/* = false*/) -> Bool
  func isInFastCOrDForm(knownWithoutScanning: Bool/* = false*/) -> Bool
}

extension UnicodeStorage
where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element {
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
  
  func contentWidth(knownWithoutScanning: Bool = false) -> UInt32 {
    return 1 << 8
  }
  func isNormalizedNFC(knownWithoutScanning: Bool = false) -> Bool {
    return true
  }
  func isNormalizedNFD(knownWithoutScanning: Bool = false) -> Bool {
    return true
  }
  func isInFastCOrDForm(knownWithoutScanning: Bool = false) -> Bool {
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
