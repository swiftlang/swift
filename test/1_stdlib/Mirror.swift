// Also run this test in optimize test modes.
// REQUIRES: optimize_test

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
// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang %S/Inputs/Mirror/Mirror.mm -c -o %t/Mirror.mm.o -g
// RUN: %target-build-swift %s -I %S/Inputs/Mirror/ -Xlinker %t/Mirror.mm.o -o %t/Mirror
// RUN: %target-run %t/Mirror

// XFAIL: linux

import StdlibUnittest

var mirrors = TestSuite("Mirrors")

extension Mirror {
  public var testDescription: String {
    let nil_ = "nil"
    return "[" + ", ".join(
      lazy(children).map { "\($0.0 ?? nil_): \(String(reflecting: $0.1))" }
    ) + "]"
  }
}

mirrors.test("RandomAccessStructure") {
  struct Eggs : CustomReflectable {
    func customMirror() -> Mirror {
      return Mirror(self, unlabeledChildren: ["aay", "bee", "cee"])
    }
  }

  let x = Eggs().customMirror()
  
  expectEqual("[nil: \"aay\", nil: \"bee\", nil: \"cee\"]", x.testDescription)
}

let letters = "abcdefghijklmnopqrstuvwxyz "

func find(substring: String, within domain: String) -> String.Index? {
  let domainCount = domain.characters.count
  let substringCount = substring.characters.count

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
    func customMirror() -> Mirror {
      return Mirror(
        self,
        unlabeledChildren: Set(letters.characters),
        displayStyle: .Set)
    }
  }

  let w = DoubleYou().customMirror()
  expectEqual(.Set, w.displayStyle)
  expectEqual(letters.characters.count, numericCast(w.children.count))
  
  // Because we don't control the order of a Set, we need to do a
  // fancy dance in order to validate the result.
  let description = w.testDescription
  for c in letters.characters {
    let expected = "nil: \"\(c)\""
    expectNotEmpty(find(expected, within: description))
  }
}

mirrors.test("BidirectionalStructure") {
  struct Why : CustomReflectable {
    func customMirror() -> Mirror {
      return Mirror(
        self,
        unlabeledChildren: letters.characters,
        displayStyle: .Collection)
    }
  }

  // Test that the basics seem to work
  let y = Why().customMirror()
  expectEqual(.Collection, y.displayStyle)

  let description = y.testDescription
  expectEqual(
    "[nil: \"a\", nil: \"b\", nil: \"c\", nil: \"",
    description[description.startIndex..<description.characters.indexOf("d")!])
}

mirrors.test("LabeledStructure") {
  struct Zee : CustomReflectable, CustomStringConvertible {
    func customMirror() -> Mirror {
      return Mirror(self, children: ["bark": 1, "bite": 0])
    }
    var description: String { return "Zee" }
  }

  let z = Zee().customMirror()
  expectEqual("[bark: 1, bite: 0]", z.testDescription)
  expectEmpty(z.displayStyle)

  struct Zee2 : CustomReflectable {
    func customMirror() -> Mirror {
      return Mirror(
        self, children: ["bark": 1, "bite": 0], displayStyle: .Dictionary)
    }
  }
  let z2 = Zee2().customMirror()
  expectEqual(.Dictionary, z2.displayStyle)
  expectEqual("[bark: 1, bite: 0]", z2.testDescription)

  struct Heterogeny : CustomReflectable {
    func customMirror() -> Mirror {
      return Mirror(
        self, children: ["bark": 1, "bite": Zee()])
    }
  }
  let h = Heterogeny().customMirror()
  expectEqual("[bark: 1, bite: Zee]", h.testDescription)
}

mirrors.test("Legacy") {
  let m = Mirror(reflecting: [1, 2, 3])
  expectTrue(m.subjectType == [Int].self)
  
  let x0: [Mirror.Child] = [
    (label: "[0]", value: 1),
    (label: "[1]", value: 2),
    (label: "[2]", value: 3)
  ]
  expectFalse(
    zip(x0, m.children).contains {
      $0.0.label != $0.1.label || $0.0.value as! Int != $0.1.value as! Int
    })

  class B { let bx: Int = 0 }
  class D : B { let dx: Int = 1 }

  let mb = Mirror(reflecting: B())
  
  func expectBMirror(
    mb: Mirror,   stackTrace: SourceLocStack? = nil,
    file: String = __FILE__, line: UWord = __LINE__
  ) {
    expectTrue(mb.subjectType == B.self,
      stackTrace: stackTrace, file: file, line: line)
    
    expectEmpty(
      mb.superclassMirror(),
      stackTrace: stackTrace, file: file, line: line)
    
    expectEqual(
      1, mb.children.count,
      stackTrace: stackTrace, file: file, line: line)
    
    expectEqual(
      "bx", mb.children.first?.label,
      stackTrace: stackTrace, file: file, line: line)
    
    expectEqual(
      0, mb.children.first?.value as? Int,
      stackTrace: stackTrace, file: file, line: line)
  }
  
  expectBMirror(mb)
  
  // Ensure that the base class instance is properly filtered out of
  // the child list
  do {
    let md = Mirror(reflecting: D())
    expectTrue(md.subjectType == D.self)
    
    expectEqual(1, md.children.count)
    expectEqual("dx", md.children.first?.label)
    expectEqual(1, md.children.first?.value as? Int)
    
    expectNotEmpty(md.superclassMirror())
    if let mb2 = md.superclassMirror() { expectBMirror(mb2) }
  }

  do {
    // Ensure that we reflect on the dynamic type of the subject
    let md = Mirror(reflecting: D() as B)
    expectTrue(md.subjectType == D.self)
    
    expectEqual(1, md.children.count)
    expectEqual("dx", md.children.first?.label)
    expectEqual(1, md.children.first?.value as? Int)
    
    expectNotEmpty(md.superclassMirror())
    if let mb2 = md.superclassMirror() { expectBMirror(mb2) }
  }
}

//===----------------------------------------------------------------------===//
//===--- Class Support ----------------------------------------------------===//

mirrors.test("Class/Root/Uncustomized") {
  class A { var a: Int = 1 }

  let a = Mirror(reflecting: A())
  expectTrue(a.subjectType == A.self)
  expectEmpty(a.superclassMirror())
  expectEqual(1, a.children.count)
  expectEqual("a", a.children.first!.label)
}

//===--- Generated Superclass Mirrors -------------------------------------===//
mirrors.test("Class/Root/superclass:.Generated") {
  class B : CustomReflectable {
    var b: String = "two"
    func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "bee": b ], ancestorRepresentation: .Generated)
    }
  }
  
  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  expectEmpty(b.superclassMirror())
  expectEqual(1, b.children.count)
  expectEqual("bee", b.children.first!.label)
  expectEqual("two", b.children.first!.value as? String)
}


mirrors.test("class/Root/superclass:<default>") {
  class C : CustomReflectable {
    var c: UInt = 3
    func customMirror() -> Mirror {
      return Mirror(self, children: [ "sea": c + 1 ])
    }
  }
  
  let c = Mirror(reflecting: C())
  expectTrue(c.subjectType == C.self)
  expectEmpty(c.superclassMirror())
  expectEqual(1, c.children.count)
  expectEqual("sea", c.children.first!.label)
  expectEqual(4, c.children.first!.value as? UInt)
}

mirrors.test("class/Plain/Plain") {
  class A { var a: Int = 1 }
  class B : A { var b: UInt = 42 }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  
  if let bChild = expectNotEmpty(b.children.first) {
    expectEqual("b", bChild.label)
    expectEqual(42, bChild.value as? UInt)
  }
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    if let aChild = expectNotEmpty(a.children.first) {
      expectEqual("a", aChild.label)
      expectEqual(1, aChild.value as? Int)
      expectEmpty(a.superclassMirror())
    }
  }
}

mirrors.test("class/UncustomizedSuper/Synthesized/Implicit") {
  class A { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    func customMirror() -> Mirror {
      return Mirror(self, children: [ "bee": b ])
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first?.label)
    expectEmpty(a.superclassMirror())
  }
}

mirrors.test("class/UncustomizedSuper/Synthesized/Explicit") {
  class A { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "bee": b ], ancestorRepresentation: .Generated)
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first!.label)
    expectEmpty(a.superclassMirror())
  }
}

mirrors.test("class/CustomizedSuper/Synthesized") {
  class A : CustomReflectable {
    var a: Int = 1
    func customMirror() -> Mirror {
      return Mirror(self, children: [ "aye": a ])
    }
  }

  class B : A {
    var b: UInt = 42
    // This is an unusual case: when writing override on a
    // customMirror implementation you would typically want to pass
    // ancestorRepresentation: .Customized(super.customMirror) or, in
    // rare cases, ancestorRepresentation: .Suppressed.  However, it
    // has an expected behavior, which we test here.
    override func customMirror() -> Mirror {
      return Mirror(self, children: [ "bee": b ])
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first!.label)
    expectEmpty(a.superclassMirror())
  }
}

import Foundation

//===--- ObjC Base Classes ------------------------------------------------===//

mirrors.test("class/ObjCPlain/Plain") {
  class A : NSObject { var a: Int = 1 }
  class B : A { var b: UInt = 42 }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  
  if let bChild = expectNotEmpty(b.children.first) {
    expectEqual("b", bChild.label)
    expectEqual(42, bChild.value as? UInt)
  }
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    if let aChild = expectNotEmpty(a.children.first) {
      expectEqual("a", aChild.label)
      expectEqual(1, aChild.value as? Int)
      if let o = expectNotEmpty(a.superclassMirror()) {
        expectEqual("NSObject", String(reflecting: o.subjectType))
      }
    }
  }
}

mirrors.test("class/ObjCUncustomizedSuper/Synthesized/Implicit") {
  class A : NSObject { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    func customMirror() -> Mirror {
      return Mirror(self, children: [ "bee": b ])
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first?.label)
    if let o = expectNotEmpty(a.superclassMirror()) {
      expectTrue(o.subjectType == NSObject.self)
    }
  }
}

mirrors.test("class/ObjCUncustomizedSuper/Synthesized/Explicit") {
  class A : NSObject { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "bee": b ], ancestorRepresentation: .Generated)
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first!.label)
    if let o = expectNotEmpty(a.superclassMirror()) {
      expectTrue(o.subjectType == NSObject.self)
    }
  }
}

mirrors.test("class/ObjCCustomizedSuper/Synthesized") {
  class A : NSDateFormatter, CustomReflectable {
    var a: Int = 1
    func customMirror() -> Mirror {
      return Mirror(self, children: [ "aye": a ])
    }
  }

  class B : A {
    var b: UInt = 42
    // This is an unusual case: when writing override on a
    // customMirror implementation you would typically want to pass
    // ancestorRepresentation: .Customized(super.customMirror) or, in
    // rare cases, ancestorRepresentation: .Suppressed.  However, it
    // has an expected behavior, which we test here.
    override func customMirror() -> Mirror {
      return Mirror(self, children: [ "bee": b ])
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first!.label)
    if let d = expectNotEmpty(a.superclassMirror()) {
      expectTrue(d.subjectType == NSDateFormatter.self)
      if let f = expectNotEmpty(d.superclassMirror()) {
        expectTrue(f.subjectType == NSFormatter.self)
        if let o = expectNotEmpty(f.superclassMirror()) {
          expectTrue(o.subjectType == NSObject.self)
          expectEmpty(o.superclassMirror())
        }
      }
    }
  }
}

//===--- Suppressed Superclass Mirrors ------------------------------------===//
mirrors.test("Class/Root/NoSuperclassMirror") {
  class B : CustomReflectable {
    var b: String = "two"
    func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "bee": b ], ancestorRepresentation: .Suppressed)
    }
  }
  
  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  expectEmpty(b.superclassMirror())
  expectEqual(1, b.children.count)
  expectEqual("bee", b.children.first!.label)
}

mirrors.test("class/UncustomizedSuper/NoSuperclassMirror") {
  class A { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "bee": b ], ancestorRepresentation: .Suppressed)
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  expectEmpty(b.superclassMirror())
}

mirrors.test("class/CustomizedSuper/NoSuperclassMirror") {
  class A : CustomReflectable {
    var a: Int = 1
    func customMirror() -> Mirror {
      return Mirror(self, children: [ "aye": a ])
    }
  }

  class B : A {
    var b: UInt = 42
    override func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "bee": b ], ancestorRepresentation: .Suppressed)
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  expectEmpty(b.superclassMirror())
}

//===--- Override Superclass Mirrors --------------------------------------===//
mirrors.test("class/CustomizedSuper/SuperclassCustomMirror/Direct") {
  class A : CustomReflectable {
    var a: Int = 1
    func customMirror() -> Mirror {
      return Mirror(self, children: [ "aye": a ])
    }
  }

  // B inherits A directly
  class B : A {
    var b: UInt = 42
    override func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "bee": b ],
        ancestorRepresentation: .Customized(super.customMirror)
        )
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    expectEqual("aye", a.children.first!.label)
    expectEmpty(a.superclassMirror())
  }
}

mirrors.test("class/CustomizedSuper/SuperclassCustomMirror/Indirect") {
  class A : CustomReflectable {
    var a: Int = 1
    func customMirror() -> Mirror {
      return Mirror(self, children: [ "aye": a ])
    }
  }

  class X : A {}

  class Y : X {}

  // B inherits A indirectly through X and Y
  class B : Y {
    var b: UInt = 42
    override func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "bee": b ],
        ancestorRepresentation: .Customized(super.customMirror))
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let y = expectNotEmpty(b.superclassMirror()) {
    expectTrue(y.subjectType == Y.self)
    if let x = expectNotEmpty(y.superclassMirror()) {
      expectTrue(x.subjectType == X.self)
      expectEqual(0, x.children.count)
      if let a = expectNotEmpty(x.superclassMirror()) {
        expectTrue(a.subjectType == A.self)
        if let aye = expectNotEmpty(a.children.first) {
          expectEqual("aye", aye.label)
        }
      }
    }
  }
}

mirrors.test("class/CustomizedSuper/SuperclassCustomMirror/Indirect2") {
  class A : CustomLeafReflectable {
    var a: Int = 1
    func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "aye": a ])
    }
  }

  class X : A {}

  class Y : X {}

  // B inherits A indirectly through X and Y
  class B : Y {
    var b: UInt = 42
    override func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "bee": b ],
        ancestorRepresentation: .Customized(super.customMirror))
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotEmpty(b.superclassMirror()) {
    expectTrue(a.subjectType == A.self)
    if let aye = expectNotEmpty(a.children.first) {
      expectEqual("aye", aye.label)
    }
  }
}

mirrors.test("class/Cluster") {
  class A : CustomLeafReflectable {
    var a: Int = 1
    func customMirror() -> Mirror {
      return Mirror(
        self, children: [ "aye": a ])
    }
  }

  class X : A {}

  class Y : X {}

  let a = Mirror(reflecting: Y())
  expectTrue(a.subjectType == A.self)
  if let aye = expectNotEmpty(a.children.first) {
    expectEqual("aye", aye.label)
  }
}

//===--- End Class Support ------------------------------------------------===//
//===----------------------------------------------------------------------===//

mirrors.test("Addressing") {
  let m0 = Mirror(reflecting: [1, 2, 3])
  expectEqual(1, m0.descendant(0) as? Int)
  expectEqual(1, m0.descendant("[0]") as? Int)
  expectEqual(2, m0.descendant(1) as? Int)
  expectEqual(2, m0.descendant("[1]") as? Int)
  expectEqual(3, m0.descendant(2) as? Int)
  expectEqual(3, m0.descendant("[2]") as? Int)
  
  let m1 = Mirror(reflecting: (a: ["one", "two", "three"], b: 4))
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
    func customMirror() -> Mirror {
      return Mirror(self, children: ["bark": 1, "bite": 0])
    }
  }
  
  let x = [
    (a: ["one", "two", "three"], b: Zee()),
    (a: ["five"], b: Zee()),
    (a: [], b: Zee())]

  let m = Mirror(reflecting: x)
  let two = m.descendant(0, ".0", 1)
  expectEqual("two", two as? String)
  expectEqual(1, m.descendant(1, 1, "bark") as? Int)
  expectEqual(0, m.descendant(1, 1, "bite") as? Int)
  expectEmpty(m.descendant(1, 1, "bork"))
}

mirrors.test("Invalid Path Type")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  struct X : MirrorPathType {}
  let m = Mirror(reflecting: [1, 2, 3])
  expectEqual(1, m.descendant(0) as? Int)
  expectCrashLater()
  m.descendant(X())
}

mirrors.test("PlaygroundQuickLook") {
  // Customization works.
  struct CustomQuickie : CustomPlaygroundQuickLookable {
    func customPlaygroundQuickLook() -> PlaygroundQuickLook {
      return .Point(1.25, 42)
    }
  }
  switch PlaygroundQuickLook(reflecting: CustomQuickie()) {
  case .Point(1.25, 42): break; default: expectTrue(false)
  }
  
  // PlaygroundQuickLook support from Legacy Mirrors works.
  switch PlaygroundQuickLook(reflecting: true) {
  case .Logical(true): break; default: expectTrue(false)
  }

  // With no Legacy Mirror QuickLook support, we fall back to
  // String(reflecting: ).
  struct X {}
  switch PlaygroundQuickLook(reflecting: X()) {
  case .Text(let text):
    expectTrue(text.hasSuffix(".(X #1)()")) { text }
  default:
    expectTrue(false)
  }
  struct Y : CustomDebugStringConvertible {
    var debugDescription: String { return "Why?" }
  }
  switch PlaygroundQuickLook(reflecting: Y()) {
  case .Text("Why?"): break; default: expectTrue(false)
  }
}

import MirrorObjC
mirrors.test("ObjC") {
  // Some Foundation classes lie about their ivars, which would crash
  // a mirror; make sure we are not automatically exposing ivars of
  // Objective-C classes from the default mirror implementation.
  expectEqual(0, Mirror(reflecting: HasIVars()).children.count)
}

mirrors.test("String.init") {
  expectEqual("42", String(42))
  expectEqual("42", String("42"))
  expectEqual("42", String(reflecting: 42))
  expectEqual("\"42\"", String(reflecting: "42"))
}
runAllTests()
