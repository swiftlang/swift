// RUN: %target-run-simple-swift

import SwiftExperimental

// The thing with the default schema
public struct Mirror {
  public typealias Child = (label: String?, value: Any)
  public typealias Structure = AnyForwardCollection<Child>
  
  // public enum Schema {
  // case Struct, Class, Enum, Optional, Array, Dictionary, Set
  // }

  public typealias Schema = MirrorDisposition
  
  public init<
    C: CollectionType where C.Generator.Element == Child
  >(structure: C, schema: Schema? = nil) {
    self.structure = Structure(structure)
    self.schema = schema
  }

  public init<
    C: CollectionType
  >(_ unlabeledChildren: C, schema: Schema? = nil) {
    self.structure = Structure(
      lazy(unlabeledChildren).map { Child(label: nil, value: $0) }
    )
    self.schema = schema
  }

  public let structure: Structure
  public let schema: Schema?
}


extension Mirror {
  internal struct LegacyChildren : CollectionType {
    init(_ oldMirror: MirrorType) {
      self._oldMirror = oldMirror
    }
    var startIndex: Int { return 0 }
    var endIndex: Int { return _oldMirror.count }
    subscript(position: Int) -> Child {
      let (label, childMirror) = _oldMirror[position]
      return (label: label, value: childMirror.value)
    }
    func generate() -> IndexingGenerator<LegacyChildren> {
      return IndexingGenerator(self)
    }
    internal let _oldMirror: MirrorType
  }
  
  public init(_ oldMirror: MirrorType) {
    self.init(
      structure: LegacyChildren(oldMirror),
      schema: oldMirror.disposition)
  }
}

public protocol CustomReflectable {
  func reflect() -> Mirror
}

func reflect(x: Any) -> Mirror {
  if let customized? = x as? CustomReflectable {
    return customized.reflect()
  }
  else {
    return Mirror(Swift.reflect(x))
  }
}

//===--- Addressing -------------------------------------------------------===//
public protocol MirrorPathType {}
extension IntMax : MirrorPathType {}
extension Int : MirrorPathType {}
extension String : MirrorPathType {}

/// Returns the first index `i` in `indices(domain)` such that
/// `predicate(domain[i])` is `true``, or `nil` if
/// `predicate(domain[i])` is `false` for all `i`.
///
/// Complexity: O(\ `count(domain)`\ )
public func find<
  C: CollectionType
>(domain: C, predicate: (C.Generator.Element)->Bool) -> C.Index? {
  for i in indices(domain) {
    if predicate(domain[i]) {
      return i
    }
  }
  return nil
}

extension Mirror {
  struct _Dummy : CustomReflectable {
    var mirror: Mirror
    func reflect() -> Mirror { return mirror }
  }
  
  func descendant(first: MirrorPathType, _ rest: MirrorPathType...) -> Any? {
    var result: Any = _Dummy(mirror: self)
    for e in [first] + rest {
      let structure = reflect(result).structure
      let position: Structure.Index
      if let label? = e as? String {
        position = find(structure) { $0.label == label } ?? structure.endIndex
      }
      else if let offset? = (e as? Int).map({ IntMax($0) }) ?? (e as? IntMax) {
        position = advance(structure.startIndex, offset, structure.endIndex)
      }
      else {
        _preconditionFailure(
          "Someone added a conformance to MirrorPathType; that privilege is reserved to the standard library")
      }
      if position == structure.endIndex { return nil }
      result = structure[position].value
    }
    return result
  }
}

//===--- Adapters ---------------------------------------------------------===//
//===----------------------------------------------------------------------===//

public struct LabeledStructure<Element>
  : CollectionType, DictionaryLiteralConvertible {
  
  public init(dictionaryLiteral elements: (String, Element)...) {
    self.elements = elements
  }

  /// Construct a copy of `other`
  ///
  /// exists primarily so a Dictionary literal can be passed as an argument
  public init(_ other: LabeledStructure) {
    self.elements = other.elements
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
      return Mirror(Set(letters), schema: .MembershipContainer)
    }
  }

  let w = DoubleYou().reflect()
  expectEqual(.MembershipContainer, w.schema)
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
      return Mirror(letters, schema: .Container)
    }
  }

  // Test that the basics seem to work
  let y = Why().reflect()
  expectEqual(.Container, y.schema)

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
        LabeledStructure(["bark": 1, "bite": 0]), schema: .KeyContainer)
    }
  }
  let z2 = Zee2().reflect()
  expectEqual(.KeyContainer, z2.schema)
  expectEqual("[bark: 1, bite: 0]", z2.description)
}

mirrors.test("Legacy") {
  let m = reflect([1, 2, 3])
  let x0: [Mirror.Child] = [
    (label: "[0]", value: 1),
    (label: "[1]", value: 2),
    (label: "[2]", value: 3)
  ]
  expectFalse(
    contains(zip(x0, m.structure)) {
      $0.0.label != $0.1.label || $0.0.value as! Int != $0.1.value as! Int
    })
}

mirrors.test("Addressing") {
  let m0 = reflect([1, 2, 3])
  expectEqual(1, m0.descendant(0) as? Int)
  expectEqual(1, m0.descendant("[0]") as? Int)
  expectEqual(2, m0.descendant(1) as? Int)
  expectEqual(2, m0.descendant("[1]") as? Int)
  expectEqual(3, m0.descendant(2) as? Int)
  expectEqual(3, m0.descendant("[2]") as? Int)
  
  let m1 = reflect((a: ["one", "two", "three"], b: 4))
  let ott0 = m1.descendant(0) as? [String]
  expectNotEmpty(ott0)
  let ott1 = m1.descendant(".0") as? [String]
  expectNotEmpty(ott1)
  if ott0 != nil && ott1 != nil {
    expectEqualSequence(ott0!, ott1!)
  }
  expectEqual(4, m1.descendant(1) as? Int)
  expectEqual(4, m1.descendant(".1") as? Int)
  expectEqual("one", m1.descendant(0, 0) as? String)
  expectEqual("two", m1.descendant(0, 1) as? String)
  expectEqual("three", m1.descendant(0, 2) as? String)
  expectEqual("one", m1.descendant(".0", 0) as? String)
  expectEqual("two", m1.descendant(0, "[1]") as? String)
  expectEqual("three", m1.descendant(".0", "[2]") as? String)

  struct Zee : CustomReflectable {
    func reflect() -> Mirror {
      return ["bark": 1, "bite": 0]
    }
  }
  
  let x = [
    (a: ["one", "two", "three"], b: Zee()),
    (a: ["five"], b: Zee()),
    (a: [], b: Zee())]

  let m = reflect(x)
  let two = m.descendant(0, ".0", 1)
  expectEqual("two", two as? String)
  expectEqual(1, m.descendant(1, 1, "bark") as? Int)
  expectEqual(0, m.descendant(1, 1, "bite") as? Int)
  expectEmpty(m.descendant(1, 1, "bork"))
}

mirrors.test("Invalid Path Type") {
  struct X : MirrorPathType {}
  let m = reflect([1, 2, 3])
  expectEqual(1, m.descendant(0) as? Int)
  expectCrashLater()
  m.descendant(X())
}

runAllTests()
