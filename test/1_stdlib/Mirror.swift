//===--- Mirror.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift

// XFAIL: linux

import SwiftExperimental
import StdlibUnittest

var mirrors = TestSuite("Mirrors")

extension Mirror: Printable {
  public var description: String {
    let nil_ = "nil"
    return "[" + ", ".join(
      lazy(children).map { "\($0.0 ?? nil_): \(toDebugString($0.1))" }
    ) + "]"
  }
}

mirrors.test("RandomAccessStructure") {
  struct Eggs : CustomReflectable {
    func makeCustomMirror() -> Mirror {
      return Mirror(unlabeledChildren: ["aay", "bee", "cee"])
    }
  }

  let x = Eggs().makeCustomMirror()
  
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
    func makeCustomMirror() -> Mirror {
      return Mirror(unlabeledChildren: Set(letters), displayStyle: .Set)
    }
  }

  let w = DoubleYou().makeCustomMirror()
  expectEqual(.Set, w.displayStyle)
  expectEqual(count(letters), numericCast(count(w.children)))
  
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
    func makeCustomMirror() -> Mirror {
      return Mirror(unlabeledChildren: letters, displayStyle: .Collection)
    }
  }

  // Test that the basics seem to work
  let y = Why().makeCustomMirror()
  expectEqual(.Collection, y.displayStyle)

  let description = y.description
  expectEqual(
    "[nil: \"a\", nil: \"b\", nil: \"c\", nil: \"",
    description[description.startIndex..<find(description, "d")!])
}

mirrors.test("LabeledStructure") {
  struct Zee : CustomReflectable, Printable {
    func makeCustomMirror() -> Mirror {
      return Mirror(children: ["bark": 1, "bite": 0])
    }
    var description: String { return "Zee" }
  }

  let z = Zee().makeCustomMirror()
  expectEqual("[bark: 1, bite: 0]", z.description)
  expectEmpty(z.displayStyle)

  struct Zee2 : CustomReflectable {
    func makeCustomMirror() -> Mirror {
      return Mirror(
        children: ["bark": 1, "bite": 0], displayStyle: .Dictionary)
    }
  }
  let z2 = Zee2().makeCustomMirror()
  expectEqual(.Dictionary, z2.displayStyle)
  expectEqual("[bark: 1, bite: 0]", z2.description)

  struct Heterogeny : CustomReflectable {
    func makeCustomMirror() -> Mirror {
      return Mirror(
        children: ["bark": 1, "bite": Zee()])
    }
  }
  let h = Heterogeny().makeCustomMirror()
  expectEqual("[bark: 1, bite: Zee]", h.description)
}

mirrors.test("Legacy") {
  let m = Mirror(reflect: [1, 2, 3])
  let x0: [Mirror.Child] = [
    (label: "[0]", value: 1),
    (label: "[1]", value: 2),
    (label: "[2]", value: 3)
  ]
  expectFalse(
    contains(zip(x0, m.children)) {
      $0.0.label != $0.1.label || $0.0.value as! Int != $0.1.value as! Int
    })
}

mirrors.test("Addressing") {
  let m0 = Mirror(reflect: [1, 2, 3])
  expectEqual(1, m0.descendant(0) as? Int)
  expectEqual(1, m0.descendant("[0]") as? Int)
  expectEqual(2, m0.descendant(1) as? Int)
  expectEqual(2, m0.descendant("[1]") as? Int)
  expectEqual(3, m0.descendant(2) as? Int)
  expectEqual(3, m0.descendant("[2]") as? Int)
  
  let m1 = Mirror(reflect: (a: ["one", "two", "three"], b: 4))
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
    func makeCustomMirror() -> Mirror {
      return Mirror(children: ["bark": 1, "bite": 0])
    }
  }
  
  let x = [
    (a: ["one", "two", "three"], b: Zee()),
    (a: ["five"], b: Zee()),
    (a: [], b: Zee())]

  let m = Mirror(reflect: x)
  let two = m.descendant(0, ".0", 1)
  expectEqual("two", two as? String)
  expectEqual(1, m.descendant(1, 1, "bark") as? Int)
  expectEqual(0, m.descendant(1, 1, "bite") as? Int)
  expectEmpty(m.descendant(1, 1, "bork"))
}

mirrors.test("Invalid Path Type") {
  struct X : MirrorPathType {}
  let m = Mirror(reflect: [1, 2, 3])
  expectEqual(1, m.descendant(0) as? Int)
  expectCrashLater()
  m.descendant(X())
}

mirrors.test("PlaygroundQuickLook") {
  // Customization works.
  struct CustomQuickie : CustomPlaygroundQuickLookable {
    func makeCustomPlaygroundQuickLook() -> PlaygroundQuickLook {
      return .Point(1.25, 42)
    }
  }
  switch PlaygroundQuickLook(reflect: CustomQuickie()) {
  case .Point(1.25, 42): break; default: expectTrue(false)
  }
  
  // PlaygroundQuickLook support from Legacy Mirrors works.
  switch PlaygroundQuickLook(reflect: true) {
  case .Logical(true): break; default: expectTrue(false)
  }

  // With no Legacy Mirror QuickLook support, we fall back to
  // toDebugString().
  struct X {}
  switch PlaygroundQuickLook(reflect: X()) {
  case .Text(let text) where text.hasSuffix(".(X #1)"): break;
  default: expectTrue(false)
  }
  struct Y : DebugPrintable {
    var debugDescription: String { return "Why?" }
  }
  switch PlaygroundQuickLook(reflect: Y()) {
  case .Text("Why?"): break; default: expectTrue(false)
  }
}

runAllTests()
