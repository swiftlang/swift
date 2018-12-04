// RUN: %target-resilience-test --backward-deployment
// REQUIRES: executable_test

import StdlibUnittest
import backward_deploy_class


var BackwardDeployClassTest = TestSuite("BackwardDeployClass")

BackwardDeployClassTest.test("ResilientClass") {
  if getVersion() == 1 {
    let s = ResilientClass()

    s.fn(s.storedProp)
    s.storedProp = 1
    s.storedProp += 1

    s.fn(s.computedProp)
    s.computedProp = 1
    s.computedProp += 1

    s.fn(s[0])
    s[0] = 1
    s[0] += 1
  }
}

BackwardDeployClassTest.test("FixedLayoutClass") {
  if getVersion() == 1 {
    let s = FixedLayoutClass()

    s.fn(s.storedProp)
    s.storedProp = 1
    s.storedProp += 1

    s.fn(s.computedProp)
    s.computedProp = 1
    s.computedProp += 1

    s.fn(s[0])
    s[0] = 1
    s[0] += 1
  }
}

BackwardDeployClassTest.test("OpenClass") {
  class DerivedClass : OpenClass {
    var count: Int = 0

    override func oldMethod() {
      count += 1
      super.oldMethod()
    }

    override func newMethod() {
      count += 10
      super.newMethod()
    }
  }

  let d = DerivedClass()

  d.oldMethod()
  if getVersion() == 1 {
    d.newMethod()
    expectEqual(d.count, 11)
  } else {
    expectEqual(d.count, 1)
  }
}

BackwardDeployClassTest.test("InsertSuperclass") {
  class DerivedClass : Top {
    var count: Int = 0

    override func bottomMethod() {
      count += 1
      super.bottomMethod()
    }

    override func middleMethod() {
      count += 10
      super.middleMethod()
    }
  }

  let d = DerivedClass()

  d.bottomMethod()
  if getVersion() == 1 {
    d.middleMethod()
    expectEqual(d.count, 11)
  } else {
    expectEqual(d.count, 1)
  }
}

runAllTests()
