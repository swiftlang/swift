// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/libresilient_struct.%target-dylib-extension) -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/libresilient_struct.%target-dylib-extension

// RUN: %target-build-swift-dylib(%t/libresilient_class.%target-dylib-extension) -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience %S/../Inputs/resilient_class.swift -emit-module -emit-module-path %t/resilient_class.swiftmodule -module-name resilient_class -I%t -L%t -lresilient_struct
// RUN: %target-codesign %t/libresilient_class.%target-dylib-extension

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -lresilient_class -o %t/main -Xfrontend -enable-class-resilience -Xlinker -rpath -Xlinker %t
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/libresilient_struct.%target-dylib-extension %t/libresilient_class.%target-dylib-extension

// RUN: %target-build-swift-dylib(%t/libresilient_struct_wmo.%target-dylib-extension) -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct -whole-module-optimization
// RUN: %target-codesign %t/libresilient_struct_wmo.%target-dylib-extension

// RUN: %target-build-swift-dylib(%t/libresilient_class_wmo.%target-dylib-extension) -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience %S/../Inputs/resilient_class.swift -emit-module -emit-module-path %t/resilient_class.swiftmodule -module-name resilient_class -I%t -L%t -lresilient_struct_wmo -whole-module-optimization
// RUN: %target-codesign %t/libresilient_class_wmo.%target-dylib-extension

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct_wmo -lresilient_class_wmo -Xfrontend -enable-class-resilience -o %t/main2 -Xlinker -rpath -Xlinker %t -module-name main
// RUN: %target-codesign %t/main2

// RUN: %target-run %t/main2 %t/libresilient_struct_wmo.%target-dylib-extension %t/libresilient_class_wmo.%target-dylib-extension

// REQUIRES: executable_test

import StdlibUnittest


import resilient_class
import resilient_struct

var ResilientClassTestSuite = TestSuite("ResilientClass")

// Concrete class with resilient stored property

protocol ProtocolWithResilientProperty {
  var s: Size { get }
}

public class ClassWithResilientProperty : ProtocolWithResilientProperty {
  public let p: Point
  public let s: Size
  public let color: Int32

  public init(p: Point, s: Size, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}

@inline(never) func getS(_ p: ProtocolWithResilientProperty) -> Size {
  return p.s
}

ResilientClassTestSuite.test("ClassWithResilientProperty") {
  let c = ClassWithResilientProperty(
      p: Point(x: 10, y: 20),
      s: Size(w: 30, h: 40),
      color: 50)

  expectEqual(c.p.x, 10)
  expectEqual(c.p.y, 20)
  expectEqual(c.s.w, 30)
  expectEqual(c.s.h, 40)
  expectEqual(c.color, 50)

  // Make sure the conformance works
  expectEqual(getS(c).w, 30)
  expectEqual(getS(c).h, 40)
}

ResilientClassTestSuite.test("OutsideClassWithResilientProperty") {
  let c = OutsideParentWithResilientProperty(
      p: Point(x: 10, y: 20),
      s: Size(w: 30, h: 40),
      color: 50)

  expectEqual(c.p.x, 10)
  expectEqual(c.p.y, 20)
  expectEqual(c.s.w, 30)
  expectEqual(c.s.h, 40)
  expectEqual(c.color, 50)

  expectEqual(0, c.laziestNumber)
  c.laziestNumber = 1
  expectEqual(1, c.laziestNumber)
}


// Generic class with resilient stored property

public class GenericClassWithResilientProperty<T> {
  public let s1: Size
  public let t: T
  public let s2: Size

  public init(s1: Size, t: T, s2: Size) {
    self.s1 = s1
    self.t = t
    self.s2 = s2
  }
}

ResilientClassTestSuite.test("GenericClassWithResilientProperty") {
  let c = GenericClassWithResilientProperty<Int32>(
      s1: Size(w: 10, h: 20),
      t: 30,
      s2: Size(w: 40, h: 50))

  expectEqual(c.s1.w, 10)
  expectEqual(c.s1.h, 20)
  expectEqual(c.t, 30)
  expectEqual(c.s2.w, 40)
  expectEqual(c.s2.h, 50)
}


// Concrete class with non-fixed size stored property

public class ClassWithResilientlySizedProperty {
  public let r: Rectangle
  public let color: Int32

  public init(r: Rectangle, color: Int32) {
    self.r = r
    self.color = color
  }
}

ResilientClassTestSuite.test("ClassWithResilientlySizedProperty") {
  let c = ClassWithResilientlySizedProperty(
      r: Rectangle(
          p: Point(x: 10, y: 20),
          s: Size(w: 30, h: 40),
          color: 50),
      color: 60)

  expectEqual(c.r.p.x, 10)
  expectEqual(c.r.p.y, 20)
  expectEqual(c.r.s.w, 30)
  expectEqual(c.r.s.h, 40)
  expectEqual(c.r.color, 50)
  expectEqual(c.color, 60)
}


// Concrete subclass of fixed-layout class with resilient stored property

public class ChildOfParentWithResilientStoredProperty : ClassWithResilientProperty {
  let enabled: Int32

  public init(p: Point, s: Size, color: Int32, enabled: Int32) {
    self.enabled = enabled
    super.init(p: p, s: s, color: color)
  }
}

ResilientClassTestSuite.test("ChildOfParentWithResilientStoredProperty") {
  let c = ChildOfParentWithResilientStoredProperty(
      p: Point(x: 10, y: 20),
      s: Size(w: 30, h: 40),
      color: 50,
      enabled: 60)

  expectEqual(c.p.x, 10)
  expectEqual(c.p.y, 20)
  expectEqual(c.s.w, 30)
  expectEqual(c.s.h, 40)
  expectEqual(c.color, 50)
  expectEqual(c.enabled, 60)
}


// Concrete subclass of fixed-layout class with resilient stored property

public class ChildOfOutsideParentWithResilientStoredProperty : OutsideParentWithResilientProperty {
  let enabled: Int32

  public init(p: Point, s: Size, color: Int32, enabled: Int32) {
    self.enabled = enabled
    super.init(p: p, s: s, color: color)
  }
}

ResilientClassTestSuite.test("ChildOfOutsideParentWithResilientStoredProperty") {
  let c = ChildOfOutsideParentWithResilientStoredProperty(
      p: Point(x: 10, y: 20),
      s: Size(w: 30, h: 40),
      color: 50,
      enabled: 60)

  expectEqual(c.p.x, 10)
  expectEqual(c.p.y, 20)
  expectEqual(c.s.w, 30)
  expectEqual(c.s.h, 40)
  expectEqual(c.color, 50)
  expectEqual(c.enabled, 60)
}


// Resilient class from a different resilience domain

ResilientClassTestSuite.test("ResilientOutsideParent") {
  let c = ResilientOutsideParent()

  expectEqual(c.property, "ResilientOutsideParent.property")
  expectEqual(c.finalProperty, "ResilientOutsideParent.finalProperty")
}


// Concrete subclass of resilient class

public class ChildOfResilientOutsideParent : ResilientOutsideParent {
  let enabled: Int32

  public init(enabled: Int32) {
    self.enabled = enabled
    super.init()
  }
}

ResilientClassTestSuite.test("ChildOfResilientOutsideParent") {
  let c = ChildOfResilientOutsideParent(enabled: 60)

  expectEqual(c.property, "ResilientOutsideParent.property")
  expectEqual(c.finalProperty, "ResilientOutsideParent.finalProperty")
  expectEqual(c.enabled, 60)
}


// Concrete subclass of resilient class

public class ChildOfResilientOutsideParentWithResilientStoredProperty : ResilientOutsideParent {
  public let p: Point
  public let s: Size
  public let color: Int32

  public init(p: Point, s: Size, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
    super.init()
  }
}

ResilientClassTestSuite.test("ChildOfResilientOutsideParentWithResilientStoredProperty") {
  let c = ChildOfResilientOutsideParentWithResilientStoredProperty(
      p: Point(x: 10, y: 20),
      s: Size(w: 30, h: 40),
      color: 50)

  expectEqual(c.property, "ResilientOutsideParent.property")
  expectEqual(c.finalProperty, "ResilientOutsideParent.finalProperty")

  expectEqual(c.p.x, 10)
  expectEqual(c.p.y, 20)
  expectEqual(c.s.w, 30)
  expectEqual(c.s.h, 40)
  expectEqual(c.color, 50)
}

class ChildWithMethodOverride : ResilientOutsideParent {
  override func getValue() -> Int {
    return 1
  }
}

ResilientClassTestSuite.test("ChildWithMethodOverride") {
  let c = ChildWithMethodOverride()
  expectEqual(c.getValue(), 1)
}

ResilientClassTestSuite.test("TypeByName") {
  expectTrue(_typeByName("main.ClassWithResilientProperty")
             == ClassWithResilientProperty.self)
  expectTrue(_typeByName("main.ClassWithResilientlySizedProperty")
             == ClassWithResilientlySizedProperty.self)
  expectTrue(_typeByName("main.ChildOfParentWithResilientStoredProperty")
             == ChildOfParentWithResilientStoredProperty.self)
  expectTrue(_typeByName("main.ChildOfOutsideParentWithResilientStoredProperty")
             == ChildOfOutsideParentWithResilientStoredProperty.self)
}


runAllTests()
