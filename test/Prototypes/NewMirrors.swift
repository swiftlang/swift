// RUN: %target-run-simple-swift

public typealias StructuralElementDescription = (label: String?, value: Any)

// The thing you have to implement
public protocol StructuralDescriptionType {
  func count() -> Int
  func descriptionForOffset(offset: Int) -> StructuralElementDescription
}

// The thing with the default schema
public struct Mirror {
  public enum Schema {
  case Struct, Class, Enum, Optional, Array, Dictionary, Set
  }

  public init(_ structure: StructuralDescriptionType, schema: Schema? = nil) {
    self.structure = structure
    self.schema = schema
  }

  public let structure: StructuralDescriptionType
  public let schema: Schema?
}

protocol CustomReflectable {
  func reflect() -> Mirror
}

//===--- Adapters ---------------------------------------------------------===//
//===----------------------------------------------------------------------===//

//===--- Ad-hoc random access collection adapter proof-of-concept ---------===//
public struct RandomAccessStructure<
  C: CollectionType where C.Index: RandomAccessIndexType
> : StructuralDescriptionType {

  public init(_ elements: C) {
    self.elements = elements
  }

  public func count() -> Int { return numericCast(Swift.count(elements)) }
  
  public func descriptionForOffset(offset: Int) -> StructuralElementDescription {
    let v = elements[elements.startIndex.advancedBy(numericCast(offset))]
    return (label: nil, value: v)
  }
  
  internal let elements: C
}

extension Mirror {
  init<
    C: CollectionType where C.Index : RandomAccessIndexType
  >(_ structure: C, schema: Schema? = nil) {
    self.init(RandomAccessStructure(structure), schema: schema)
  }
}

//===--- Horrible ad-hoc bidirectional collection proof-of-concept --------===//
// Forward collections left as an exercise for the reader.
public struct BidirectionalStructure<
  C: CollectionType where C.Index: BidirectionalIndexType
> : StructuralDescriptionType {

  public init(_ elements: C) {
    self.elements = elements
    storedCount = numericCast(Swift.count(elements)) as Int
    var (cachedOffset, cachedIndex) = (0, elements.startIndex)
    self.mapIndex = { [storedCount = self.storedCount]
      (offset)->C.Index
    in
      let result: C.Index
      let cacheDistance = offset - cachedOffset
      switch minElement(
        [offset, abs(cacheDistance), storedCount - offset]
      ) {
      case offset:
        result = advance(elements.startIndex, numericCast(offset))
      case abs(cacheDistance):
        result = advance(cachedIndex, numericCast(cacheDistance))
      case storedCount - offset:
        result = advance(elements.endIndex, numericCast(offset - storedCount))
      default:
        fatalError("unreachable")
      }
      cachedOffset = offset
      cachedIndex = result
      return result
    }
  }
  
  public func descriptionForOffset(offset: Int) -> StructuralElementDescription {
    let v = elements[mapIndex(offset)]
    return (label: nil, value: v)
  }

  public func count() -> Int {
    return storedCount
  }
  
  internal let elements: C
  internal let storedCount: Int
  internal let mapIndex: (Int)->C.Index
}

extension Mirror {
  init<
    C: CollectionType where C.Index : BidirectionalIndexType
  >(_ structure: C, schema: Schema? = nil) {
    self.init(BidirectionalStructure(structure), schema: schema)
  }
}

//===--- Horrible ad-hoc bidirectional collection adapter proof-of-concept. ---===//
public struct ForwardStructure<
  C: CollectionType where C.Index: ForwardIndexType
> : StructuralDescriptionType {

  public init(_ elements: C) {
    self.elements = elements
    storedCount = numericCast(Swift.count(elements)) as Int
    var (cachedOffset, cachedIndex) = (0, elements.startIndex)
    self.mapIndex = { [storedCount = self.storedCount]
      (offset)->C.Index
    in
      let result: C.Index
      let cacheDistance = offset - cachedOffset
      if offset > cachedOffset {
        result = advance(cachedIndex, numericCast(offset - cachedOffset))
      }
      else if offset == storedCount {
        result = elements.endIndex
      }
      else {
        result = advance(elements.startIndex, numericCast(offset))
      }
      cachedOffset = offset
      cachedIndex = result
      return result
    }
  }
  
  public func descriptionForOffset(offset: Int) -> StructuralElementDescription {
    let v = elements[mapIndex(offset)]
    return (label: nil, value: v)
  }

  public func count() -> Int {
    return storedCount
  }
  
  internal let elements: C
  internal let storedCount: Int
  internal let mapIndex: (Int)->C.Index
}

extension Mirror {
  init<
    C: CollectionType where C.Index : ForwardIndexType
  >(_ structure: C, schema: Schema? = nil) {
    self.init(ForwardStructure(structure), schema: schema)
  }
}

//===--- StructuralDescriptionType for Aggregates -----------------------------===//
public struct LabeledStructure
  : StructuralDescriptionType, DictionaryLiteralConvertible {
  public init(dictionaryLiteral elements: (String, Any)...) {
    self.elements = elements
  }

  internal init(_ elements: [(String, Any)]) {
    self.elements = elements
  }

  public func count() -> Int { return elements.count }
  
  public func descriptionForOffset(offset: Int) -> StructuralElementDescription {
    return (label: elements[offset].0, value: elements[offset].1)
  }
  
  internal let elements: [(String, Any)]
}

extension Mirror : DictionaryLiteralConvertible {
  typealias Key = String
  typealias Value = Any
  public init(dictionaryLiteral elements: (String, Any)...) {
    self.init(LabeledStructure(elements))
  }
  
  public init(_ structure: LabeledStructure, schema: Schema? = nil) {
    self.structure = structure
    self.schema = schema
  }
}

//===--- tests ------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
import StdlibUnittest

var mirrors = TestSuite("Mirrors")

extension Mirror: Printable {
  public var description: String {
    return "[" + ", ".join(
      (0..<self.structure.count()).map {
      (offset: Int)->String in
        let d = self.structure.descriptionForOffset(offset)
        let key = d.0 ?? "nil"
        return "\(key): \(toDebugString(d.1))"
      }
    ) + "]"
  }
}

mirrors.test("RandomAccessStructure") {
  struct Eggs : CustomReflectable {
    func reflect() -> Mirror {
      return Mirror(["aay", "bee", "cee"])
    }
  }

  let x = Eggs().reflect()
  
  expectEqual("[nil: \"aay\", nil: \"bee\", nil: \"cee\"]", x.description)
}

let letters = "abcdefghijklmnopqrstuvwxyz "

func find(substring: String, within domain: String) -> String.Index? {
  let domainCount = count(domain)
  let substringCount = count(substring)

  if (domainCount < substringCount) { return nil }
  var sliceStart = domain.startIndex
  var sliceEnd = advance(domain.startIndex, substringCount)
  for var i = 0;; ++i  {
    if domain[sliceStart..<sliceEnd] == substring {
      return sliceStart
    }
    if i == domainCount - substringCount { break }
    ++sliceStart
    ++sliceEnd
  }
  return nil
}

mirrors.test("ForwardStructure") {
  struct DoubleYou : CustomReflectable {
    func reflect() -> Mirror {
      return Mirror(Set(letters), schema: .Set)
    }
  }

  let w = DoubleYou().reflect()
  expectEqual(.Set, w.schema)
  expectEqual(count(letters), w.structure.count())
  
  // Because we don't control the order of a Set, we need to do a
  // fancy dance in order to validate the result.
  let description = w.description
  for c in letters {
    let expected = "nil: \"\(c)\""
    expectNotEmpty(find(expected, within: description))
  }
}

mirrors.test("BidirectionalStructure") {
  struct Why : CustomReflectable {
    func reflect() -> Mirror {
      return Mirror(letters, schema: .Array)
    }
  }

  // Test that the basics seem to work
  let y = Why().reflect()
  expectEqual(.Array, y.schema)

  let description = y.description
  expectEqual(
    "[nil: \"a\", nil: \"b\", nil: \"c\", nil: \"",
    description[description.startIndex..<find(description, "d")!])

  // Now test something that demands jumping around to make sure the
  // index cache isn't returning bogus results
  let s = y.structure
  let sentence = "the quick brown fox jumped over the lazy dog"
  
  // Get offsets in letters for each element of sentence
  let letterOffsets = map(sentence) {
    [letters = Array(letters)] index in find(letters, index)!
  }

  // Use these offsets to reconstruct the sentence from the structural
  // description of y
  let recovered = "".join(
    letterOffsets.map { String( s.descriptionForOffset($0).1 as! Character )})

  expectEqual(sentence, recovered)
}

mirrors.test("LabeledStructure") {
  struct Zee : CustomReflectable {
    func reflect() -> Mirror {
      return ["bark": 1, "bite": 0]
    }
  }

  let z = Zee().reflect()
  expectEqual("[bark: 1, bite: 0]", z.description)
  expectEmpty(z.schema)

  struct Zee2 : CustomReflectable {
    func reflect() -> Mirror {
      return Mirror(
        ["bark": 1, "bite": 0] as LabeledStructure, schema: .Dictionary)
    }
  }
  let z2 = Zee2().reflect()
  expectEqual(.Dictionary, z2.schema)
  expectEqual("[bark: 1, bite: 0]", z2.description)
}


runAllTests()
