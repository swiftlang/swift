// RUN: rm -rf %t && mkdir %t

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -c %S/../Inputs/resilient_struct.swift -o %t/resilient_struct.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -c %S/../Inputs/resilient_struct.swift -o %t/resilient_struct.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -c %S/../Inputs/resilient_class.swift -I %t/ -o %t/resilient_class.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -c %S/../Inputs/resilient_class.swift -I %t/ -o %t/resilient_class.o

// RUN: %target-build-swift %s -Xlinker %t/resilient_struct.o -Xlinker %t/resilient_class.o -I %t -L %t -o %t/main

// RUN: %target-run %t/main

// XFAIL: linux

import StdlibUnittest
import resilient_class
import resilient_struct

var ResilientClassTestSuite = TestSuite("ResilientClass")

// Concrete class with resilient stored property

public class ClassWithResilientProperty {
  public let p: Point
  public let s: Size
  public let color: Int32

  public init(p: Point, s: Size, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
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

runAllTests()
