// RUN: %target-run-simple-swift

import SwiftExperimental

// The thing with the default schema
public struct Mirror {
  public typealias Child = (label: String?, value: Any)
  public typealias Structure = AnyForwardCollection<Child>
  
  public enum Schema {
  case Struct, Class, Enum, Optional, Array, Dictionary, Set
  }

  public init(structure: Structure, schema: Schema? = nil) {
    self.structure = structure
    self.schema = schema
  }

  public init<C: CollectionType>(_ unlabeledChildren: C, schema: Schema? = nil) {
    self.structure = Structure(
      lazy(unlabeledChildren).map { Child(label: nil, value: $0) }
    )
    self.schema = schema
  }

  public let structure: Structure
  public let schema: Schema?
}

protocol CustomReflectable {
  func reflect() -> Mirror
}

//===--- Adapters ---------------------------------------------------------===//
//===----------------------------------------------------------------------===//

public struct LabeledStructure<Element>
  : CollectionType, DictionaryLiteralConvertible {
  
  public init(dictionaryLiteral elements: (String, Element)...) {
    self.elements = elements
  }
  
  public init(_ elements: [(String, Element)]) {
    self.elements = elements
  }
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return elements.endIndex }
  
  public subscript(position: Int) -> Mirror.Child {
    return (label: elements[position].0, value: elements[position].1)
  }

  public func generate() -> IndexingGenerator<LabeledStructure> {
    return IndexingGenerator(self)
  }
  
  internal let elements: [(String, Element)]
}

extension Mirror : DictionaryLiteralConvertible {
  typealias Key = String
  typealias Value = Any
  public init(dictionaryLiteral elements: (String, Any)...) {
    self.init(LabeledStructure(elements))
  }
  
  public init<Element>(
    _ structure: LabeledStructure<Element>, schema: Schema? = nil
  ) {
    self.structure = Structure(structure)
    self.schema = schema
  }
}

//===--- tests ------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
import StdlibUnittest

var mirrors = TestSuite("Mirrors")

extension Mirror: Printable {
  public var description: String {
    let nil_ = "nil"
    return "[" + ", ".join(
      lazy(structure).map { "\($0.0 ?? nil_): \(toDebugString($0.1))" }
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
  expectEqual(count(letters), numericCast(count(w.structure)))
  
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
