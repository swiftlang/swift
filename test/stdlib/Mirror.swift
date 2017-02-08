//===--- Mirror.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: if [ %target-runtime == "objc" ]; \
// RUN: then \
// RUN:   %target-clang %S/Inputs/Mirror/Mirror.mm -c -o %t/Mirror.mm.o -g && \
// RUN:   %target-build-swift %s -I %S/Inputs/Mirror/ -Xlinker %t/Mirror.mm.o -o %t/Mirror; \
// RUN: else \
// RUN:   %target-build-swift %s -o %t/Mirror; \
// RUN: fi
// RUN: %target-run %t/Mirror
// REQUIRES: executable_test

// FIXME: rdar://30332105 LLVM miscompile of vector instructions
//   in optimized build of Mirrors.LabeledStructure
// UNSUPPORTED: CPU=armv7s

import StdlibUnittest


var mirrors = TestSuite("Mirrors")

extension Mirror {
  public var testDescription: String {
    let nil_ = "nil"
    return "[" +
      children.lazy
        .map { "\($0.0 ?? nil_): \(String(reflecting: $0.1))" }
        .joined(separator: ", ")
      + "]"
  }
}

mirrors.test("RandomAccessStructure") {
  struct Eggs : CustomReflectable {
    var customMirror: Mirror {
      return Mirror(self, unlabeledChildren: ["aay", "bee", "cee"])
    }
  }

  let x = Eggs().customMirror
  
  expectEqual("[nil: \"aay\", nil: \"bee\", nil: \"cee\"]", x.testDescription)
}

let letters = "abcdefghijklmnopqrstuvwxyz "

func find(_ substring: String, within domain: String) -> String.Index? {
  let domainCount = domain.characters.count
  let substringCount = substring.characters.count

  if (domainCount < substringCount) { return nil }
  var sliceStart = domain.startIndex
  var sliceEnd = domain.index(sliceStart, offsetBy: substringCount)
  var i = 0
  while true {
    if domain[sliceStart..<sliceEnd] == substring {
      return sliceStart
    }
    if i == domainCount - substringCount { break }
    sliceStart = domain.index(after: sliceStart)
    sliceEnd = domain.index(after: sliceEnd)
    i += 1
  }
  return nil
}

mirrors.test("ForwardStructure") {
  struct DoubleYou : CustomReflectable {
    var customMirror: Mirror {
      return Mirror(
        self,
        unlabeledChildren: Set(letters.characters),
        displayStyle: .`set`)
    }
  }

  let w = DoubleYou().customMirror
  expectEqual(.`set`, w.displayStyle)
  expectEqual(letters.characters.count, numericCast(w.children.count))
  
  // Because we don't control the order of a Set, we need to do a
  // fancy dance in order to validate the result.
  let description = w.testDescription
  for c in letters.characters {
    let expected = "nil: \"\(c)\""
    expectNotNil(find(expected, within: description))
  }
}

mirrors.test("BidirectionalStructure") {
  struct Why : CustomReflectable {
    var customMirror: Mirror {
      return Mirror(
        self,
        unlabeledChildren: letters.characters,
        displayStyle: .collection)
    }
  }

  // Test that the basics seem to work
  let y = Why().customMirror
  expectEqual(.`collection`, y.displayStyle)

  let description = y.testDescription
  expectEqual(
    "[nil: \"a\", nil: \"b\", nil: \"c\", nil: \"",
    description[description.startIndex..<description.characters.index(of: "d")!])
}

mirrors.test("LabeledStructure") {
  struct Zee : CustomReflectable, CustomStringConvertible {
    var customMirror: Mirror {
      return Mirror(self, children: ["bark": 1, "bite": 0])
    }
    var description: String { return "Zee" }
  }

  let z = Zee().customMirror
  expectEqual("[bark: 1, bite: 0]", z.testDescription)
  expectNil(z.displayStyle)

  struct Zee2 : CustomReflectable {
    var customMirror: Mirror {
      return Mirror(
        self, children: ["bark": 1, "bite": 0], displayStyle: .dictionary)
    }
  }
  let z2 = Zee2().customMirror
  expectEqual(.dictionary, z2.displayStyle)
  expectEqual("[bark: 1, bite: 0]", z2.testDescription)

  struct Heterogeny : CustomReflectable {
    var customMirror: Mirror {
      return Mirror(
        self, children: ["bark": 1, "bite": Zee()])
    }
  }
  let h = Heterogeny().customMirror
  expectEqual("[bark: 1, bite: Zee]", h.testDescription)
}

mirrors.test("Legacy") {
  let m = Mirror(reflecting: [1, 2, 3])
  expectTrue(m.subjectType == [Int].self)
  
  let x0: [Mirror.Child] = [
    (label: nil, value: 1),
    (label: nil, value: 2),
    (label: nil, value: 3)
  ]
  expectFalse(
    zip(x0, m.children).contains {
      $0.0.value as! Int != $0.1.value as! Int
    })

  class B { let bx: Int = 0 }
  class D : B { let dx: Int = 1 }

  let mb = Mirror(reflecting: B())
  
  func expectBMirror(
    _ mb: Mirror, stackTrace: SourceLocStack = SourceLocStack(),
    file: String = #file, line: UInt = #line
  ) {
    expectTrue(mb.subjectType == B.self,
      stackTrace: stackTrace, file: file, line: line)
    
    expectNil(
      mb.superclassMirror,
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
    
    expectNotNil(md.superclassMirror)
    if let mb2 = md.superclassMirror { expectBMirror(mb2) }
  }

  do {
    // Ensure that we reflect on the dynamic type of the subject
    let md = Mirror(reflecting: D() as B)
    expectTrue(md.subjectType == D.self)
    
    expectEqual(1, md.children.count)
    expectEqual("dx", md.children.first?.label)
    expectEqual(1, md.children.first?.value as? Int)
    
    expectNotNil(md.superclassMirror)
    if let mb2 = md.superclassMirror { expectBMirror(mb2) }
  }
}

//===----------------------------------------------------------------------===//
//===--- Class Support ----------------------------------------------------===//

mirrors.test("Class/Root/Uncustomized") {
  class A { var a: Int = 1 }

  let a = Mirror(reflecting: A())
  expectTrue(a.subjectType == A.self)
  expectNil(a.superclassMirror)
  expectEqual(1, a.children.count)
  expectEqual("a", a.children.first!.label)
}

//===--- Generated Superclass Mirrors -------------------------------------===//
mirrors.test("Class/Root/superclass:.generated") {
  class B : CustomReflectable {
    var b: String = "two"
    var customMirror: Mirror {
      return Mirror(
        self, children: ["bee": b], ancestorRepresentation: .generated)
    }
  }
  
  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  expectNil(b.superclassMirror)
  expectEqual(1, b.children.count)
  expectEqual("bee", b.children.first!.label)
  expectEqual("two", b.children.first!.value as? String)
}


mirrors.test("class/Root/superclass:<default>") {
  class C : CustomReflectable {
    var c: UInt = 3
    var customMirror: Mirror {
      return Mirror(self, children: ["sea": c + 1])
    }
  }
  
  let c = Mirror(reflecting: C())
  expectTrue(c.subjectType == C.self)
  expectNil(c.superclassMirror)
  expectEqual(1, c.children.count)
  expectEqual("sea", c.children.first!.label)
  expectEqual(4, c.children.first!.value as? UInt)
}

mirrors.test("class/Plain/Plain") {
  class A { var a: Int = 1 }
  class B : A { var b: UInt = 42 }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  
  if let bChild = expectNotNil(b.children.first) {
    expectEqual("b", bChild.label)
    expectEqual(42, bChild.value as? UInt)
  }
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    if let aChild = expectNotNil(a.children.first) {
      expectEqual("a", aChild.label)
      expectEqual(1, aChild.value as? Int)
      expectNil(a.superclassMirror)
    }
  }
}

mirrors.test("class/UncustomizedSuper/Synthesized/Implicit") {
  class A { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    var customMirror: Mirror {
      return Mirror(self, children: ["bee": b])
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first?.label)
    expectNil(a.superclassMirror)
  }
}

mirrors.test("class/UncustomizedSuper/Synthesized/Explicit") {
  class A { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    var customMirror: Mirror {
      return Mirror(
        self, children: ["bee": b], ancestorRepresentation: .generated)
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first!.label)
    expectNil(a.superclassMirror)
  }
}

mirrors.test("class/CustomizedSuper/Synthesized") {
  class A : CustomReflectable {
    var a: Int = 1
    var customMirror: Mirror {
      return Mirror(self, children: ["aye": a])
    }
  }

  class B : A {
    var b: UInt = 42
    // This is an unusual case: when writing override on a
    // customMirror implementation you would typically want to pass
    // ancestorRepresentation: .customized(super.customMirror) or, in
    // rare cases, ancestorRepresentation: .Suppressed.  However, it
    // has an expected behavior, which we test here.
    override var customMirror: Mirror {
      return Mirror(self, children: ["bee": b])
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first!.label)
    expectNil(a.superclassMirror)
  }
}

#if _runtime(_ObjC)
import Foundation

//===--- ObjC Base Classes ------------------------------------------------===//

mirrors.test("class/ObjCPlain/Plain") {
  class A : NSObject { var a: Int = 1 }
  class B : A { var b: UInt = 42 }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  
  if let bChild = expectNotNil(b.children.first) {
    expectEqual("b", bChild.label)
    expectEqual(42, bChild.value as? UInt)
  }
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    if let aChild = expectNotNil(a.children.first) {
      expectEqual("a", aChild.label)
      expectEqual(1, aChild.value as? Int)
      if let o = expectNotNil(a.superclassMirror) {
        expectEqual("NSObject", String(reflecting: o.subjectType))
      }
    }
  }
}

mirrors.test("class/ObjCUncustomizedSuper/Synthesized/Implicit") {
  class A : NSObject { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    var customMirror: Mirror {
      return Mirror(self, children: ["bee": b])
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first?.label)
    if let o = expectNotNil(a.superclassMirror) {
      expectTrue(o.subjectType == NSObject.self)
    }
  }
}

mirrors.test("class/ObjCUncustomizedSuper/Synthesized/Explicit") {
  class A : NSObject { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    var customMirror: Mirror {
      return Mirror(
        self, children: ["bee": b], ancestorRepresentation: .generated)
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first!.label)
    if let o = expectNotNil(a.superclassMirror) {
      expectTrue(o.subjectType == NSObject.self)
    }
  }
}

mirrors.test("class/ObjCCustomizedSuper/Synthesized") {
  class A : DateFormatter, CustomReflectable {
    var a: Int = 1
    var customMirror: Mirror {
      return Mirror(self, children: ["aye": a])
    }
  }

  class B : A {
    var b: UInt = 42
    // This is an unusual case: when writing override on a
    // customMirror implementation you would typically want to pass
    // ancestorRepresentation: .customized(super.customMirror) or, in
    // rare cases, ancestorRepresentation: .Suppressed.  However, it
    // has an expected behavior, which we test here.
    override var customMirror: Mirror {
      return Mirror(self, children: ["bee": b])
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    expectEqual("a", a.children.first!.label)
    if let d = expectNotNil(a.superclassMirror) {
      expectTrue(d.subjectType == DateFormatter.self)
      if let f = expectNotNil(d.superclassMirror) {
        expectTrue(f.subjectType == Formatter.self)
        if let o = expectNotNil(f.superclassMirror) {
          expectTrue(o.subjectType == NSObject.self)
          expectNil(o.superclassMirror)
        }
      }
    }
  }
}
#endif // _runtime(_ObjC)

//===--- Suppressed Superclass Mirrors ------------------------------------===//
mirrors.test("Class/Root/NoSuperclassMirror") {
  class B : CustomReflectable {
    var b: String = "two"
    var customMirror: Mirror {
      return Mirror(
        self, children: ["bee": b], ancestorRepresentation: .suppressed)
    }
  }
  
  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  expectNil(b.superclassMirror)
  expectEqual(1, b.children.count)
  expectEqual("bee", b.children.first!.label)
}

mirrors.test("class/UncustomizedSuper/NoSuperclassMirror") {
  class A { var a: Int = 1 }

  class B : A, CustomReflectable {
    var b: UInt = 42
    var customMirror: Mirror {
      return Mirror(
        self, children: ["bee": b], ancestorRepresentation: .suppressed)
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  expectNil(b.superclassMirror)
}

mirrors.test("class/CustomizedSuper/NoSuperclassMirror") {
  class A : CustomReflectable {
    var a: Int = 1
    var customMirror: Mirror {
      return Mirror(self, children: ["aye": a])
    }
  }

  class B : A {
    var b: UInt = 42
    override var customMirror: Mirror {
      return Mirror(
        self, children: ["bee": b], ancestorRepresentation: .suppressed)
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  expectNil(b.superclassMirror)
}

//===--- Override Superclass Mirrors --------------------------------------===//
mirrors.test("class/CustomizedSuper/SuperclassCustomMirror/Direct") {
  class A : CustomReflectable {
    var a: Int = 1
    var customMirror: Mirror {
      return Mirror(self, children: ["aye": a])
    }
  }

  // B inherits A directly
  class B : A {
    var b: UInt = 42
    override var customMirror: Mirror {
      return Mirror(
        self,
        children: ["bee": b],
        ancestorRepresentation: .customized({ super.customMirror }))
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    expectEqual("aye", a.children.first!.label)
    expectNil(a.superclassMirror)
  }
}

mirrors.test("class/CustomizedSuper/SuperclassCustomMirror/Indirect") {
  class A : CustomReflectable {
    var a: Int = 1
    var customMirror: Mirror {
      return Mirror(self, children: ["aye": a])
    }
  }

  class X : A {}

  class Y : X {}

  // B inherits A indirectly through X and Y
  class B : Y {
    var b: UInt = 42
    override var customMirror: Mirror {
      return Mirror(
        self,
        children: ["bee": b],
        ancestorRepresentation: .customized({ super.customMirror }))
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let y = expectNotNil(b.superclassMirror) {
    expectTrue(y.subjectType == Y.self)
    if let x = expectNotNil(y.superclassMirror) {
      expectTrue(x.subjectType == X.self)
      expectEqual(0, x.children.count)
      if let a = expectNotNil(x.superclassMirror) {
        expectTrue(a.subjectType == A.self)
        if let aye = expectNotNil(a.children.first) {
          expectEqual("aye", aye.label)
        }
      }
    }
  }
}

mirrors.test("class/CustomizedSuper/SuperclassCustomMirror/Indirect2") {
  class A : CustomLeafReflectable {
    var a: Int = 1
    var customMirror: Mirror {
      return Mirror(
        self, children: ["aye": a])
    }
  }

  class X : A {}

  class Y : X {}

  // B inherits A indirectly through X and Y
  class B : Y {
    var b: UInt = 42
    override var customMirror: Mirror {
      return Mirror(
        self,
        children: ["bee": b],
        ancestorRepresentation: .customized({ super.customMirror }))
    }
  }

  let b = Mirror(reflecting: B())
  expectTrue(b.subjectType == B.self)
  if let a = expectNotNil(b.superclassMirror) {
    expectTrue(a.subjectType == A.self)
    if let aye = expectNotNil(a.children.first) {
      expectEqual("aye", aye.label)
    }
  }
}

mirrors.test("class/Cluster") {
  class A : CustomLeafReflectable {
    var a: Int = 1
    var customMirror: Mirror {
      return Mirror(
        self, children: ["aye": a])
    }
  }

  class X : A {}

  class Y : X {}

  let a = Mirror(reflecting: Y())
  expectTrue(a.subjectType == A.self)
  if let aye = expectNotNil(a.children.first) {
    expectEqual("aye", aye.label)
  }
}

//===--- End Class Support ------------------------------------------------===//
//===----------------------------------------------------------------------===//

mirrors.test("Addressing") {
  let m0 = Mirror(reflecting: [1, 2, 3])
  expectEqual(1, m0.descendant(0) as? Int)
  expectEqual(2, m0.descendant(1) as? Int)
  expectEqual(3, m0.descendant(2) as? Int)
  
  let m1 = Mirror(reflecting: (a: ["one", "two", "three"], 4))
  let ott0 = m1.descendant(0) as? [String]
  expectNotNil(ott0)
  let ott1 = m1.descendant("a") as? [String]
  expectNotNil(ott1)
  if ott0 != nil && ott1 != nil {
    expectEqualSequence(ott0!, ott1!)
  }
  expectEqual(4, m1.descendant(1) as? Int)
  expectEqual(4, m1.descendant(".1") as? Int)
  expectEqual("one", m1.descendant(0, 0) as? String)
  expectEqual("two", m1.descendant(0, 1) as? String)
  expectEqual("three", m1.descendant(0, 2) as? String)
  expectEqual("one", m1.descendant("a", 0) as? String)

  struct Zee : CustomReflectable {
    var customMirror: Mirror {
      return Mirror(self, children: ["bark": 1, "bite": 0])
    }
  }
  
  let x = [
    (a: ["one", "two", "three"], b: Zee()),
    (a: ["five"], b: Zee()),
    (a: [], b: Zee())]

  let m = Mirror(reflecting: x)
  let two = m.descendant(0, "a", 1)
  expectEqual("two", two as? String)
  expectEqual(1, m.descendant(1, 1, "bark") as? Int)
  expectEqual(0, m.descendant(1, 1, "bite") as? Int)
  expectNil(m.descendant(1, 1, "bork"))
}

mirrors.test("Invalid Path Type")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  struct X : MirrorPath {}
  let m = Mirror(reflecting: [1, 2, 3])
  expectEqual(1, m.descendant(0) as? Int)
  expectCrashLater()
  _ = m.descendant(X())
}

mirrors.test("PlaygroundQuickLook") {
  // Customization works.
  struct CustomQuickie : CustomPlaygroundQuickLookable {
    var customPlaygroundQuickLook: PlaygroundQuickLook {
      return .point(1.25, 42)
    }
  }
  switch PlaygroundQuickLook(reflecting: CustomQuickie()) {
  case .point(1.25, 42): break
  default: expectTrue(false)
  }
  
  // PlaygroundQuickLook support from Legacy Mirrors works.
  switch PlaygroundQuickLook(reflecting: true) {
  case .bool(true): break
  default: expectTrue(false)
  }

  // With no Legacy Mirror QuickLook support, we fall back to
  // String(reflecting: ).
  struct X {}
  switch PlaygroundQuickLook(reflecting: X()) {
  case .text(let text):
#if _runtime(_ObjC)
// FIXME: Enable if non-objc hasSuffix is implemented.
    expectTrue(text.hasSuffix(".(X #1)()"), text)
#endif
  default:
    expectTrue(false)
  }
  struct Y : CustomDebugStringConvertible {
    var debugDescription: String { return "Why?" }
  }
  switch PlaygroundQuickLook(reflecting: Y()) {
  case .text("Why?"): break
  default: expectTrue(false)
  }
}

class Parent {}

extension Parent : _DefaultCustomPlaygroundQuickLookable {
  var _defaultCustomPlaygroundQuickLook: PlaygroundQuickLook {
    return .text("base")
  }
}

class Child : Parent { }

class FancyChild : Parent, CustomPlaygroundQuickLookable {
  var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text("child")
  }
}

mirrors.test("_DefaultCustomPlaygroundQuickLookable") {
  // testing the workaround for custom quicklookables in subclasses
  switch PlaygroundQuickLook(reflecting: Child()) {
  case .text("base"): break
  default: expectUnreachable("Base custom quicklookable was expected")
  }

  switch PlaygroundQuickLook(reflecting: FancyChild()) {
  case .text("child"): break
  default: expectUnreachable("FancyChild custom quicklookable was expected")
  }
}

#if _runtime(_ObjC)
import MirrorObjC
mirrors.test("ObjC") {
  // Some Foundation classes lie about their ivars, which would crash
  // a mirror; make sure we are not automatically exposing ivars of
  // Objective-C classes from the default mirror implementation.
  expectEqual(0, Mirror(reflecting: HasIVars()).children.count)
}
#endif

mirrors.test("String.init") {
  expectEqual("42", String(42))
  expectEqual("42", String("42"))
  expectEqual("42", String(reflecting: 42))
  expectEqual("\"42\"", String(reflecting: "42"))
}
runAllTests()
