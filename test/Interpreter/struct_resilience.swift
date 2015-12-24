// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -c %S/../Inputs/resilient_struct.swift -o %t/resilient_struct.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -c %S/../Inputs/resilient_struct.swift -o %t/resilient_struct.o
// RUN: %target-build-swift %s -Xlinker %t/resilient_struct.o -I %t -L %t -o %t/main
// RUN: %target-run %t/main

import StdlibUnittest
import resilient_struct

var ResilientStructTestSuite = TestSuite("ResilientStruct")

ResilientStructTestSuite.test("ResilientValue") {
  for b in [false, true] {
    let r = ResilientBool(b: b)
    expectEqual(b, r.b)
  }
  for i in [-12, -1, 12] {
    let r = ResilientInt(i: i)
    expectEqual(i, r.i)
  }
  for d in [-1.0, 0.0, -0.0, 1.0] {
    let r = ResilientDouble(d: d)
    expectEqual(d, r.d)
  }
}

ResilientStructTestSuite.test("StaticLayout") {
  for b1 in [false, true] {
    for i in [-12, -1, 12] {
      for b2 in [false, true] {
        for d in [-1.0, 0.0, -0.0, 1.0] {
          let r = ResilientLayoutRuntimeTest(b1: ResilientBool(b: b1),
                                             i: ResilientInt(i: i),
                                             b2: ResilientBool(b: b2),
                                             d: ResilientDouble(d: d))
          expectEqual(b1, r.b1.b)
          expectEqual(i,  r.i.i)
          expectEqual(b2, r.b2.b)
          expectEqual(d,  r.d.d)
        }
      }
    }
  }
}

struct MyResilientLayoutRuntimeTest {
  let b1: ResilientBool
  let i: ResilientInt
  let b2: ResilientBool
  let d: ResilientDouble

  init(b1: ResilientBool, i: ResilientInt, b2: ResilientBool, d: ResilientDouble) {
    self.b1 = b1
    self.i = i
    self.b2 = b2
    self.d = d
  }
}

@inline(never) func getMetadata() -> Any.Type {
  return MyResilientLayoutRuntimeTest.self
}

ResilientStructTestSuite.test("DynamicLayoutMetatype") {
  do {
    var output = ""
    let expected = "- main.MyResilientLayoutRuntimeTest #0\n"
    dump(getMetadata(), &output)
    expectEqual(output, expected)
  }
  do {
    expectEqual(true, getMetadata() == getMetadata())
  }
}

ResilientStructTestSuite.test("DynamicLayout") {
  for b1 in [false, true] {
    for i in [-12, -1, 12] {
      for b2 in [false, true] {
        for d in [-1.0, 0.0, -0.0, 1.0] {
          let r = MyResilientLayoutRuntimeTest(b1: ResilientBool(b: b1),
                                               i: ResilientInt(i: i),
                                               b2: ResilientBool(b: b2),
                                               d: ResilientDouble(d: d))
          expectEqual(b1, r.b1.b)
          expectEqual(i,  r.i.i)
          expectEqual(b2, r.b2.b)
          expectEqual(d,  r.d.d)
        }
      }
    }
  }
}

runAllTests()
