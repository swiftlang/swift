// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import superclass_methods

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var SuperclassMethodsTest = TestSuite("SuperclassMethods")

SuperclassMethodsTest.test("AddInterposingMethod") {
  do {
    class Leaf : AddInterposingMethod {
      override func method() -> String {
        return super.method()
      }
      override class func classMethod() -> String {
        return super.classMethod()
      }
    }
    if getVersion() == 0 {
      expectEqual(Leaf().method(), "Base.method()")
      expectEqual(Leaf.classMethod(), "Base.classMethod()")
    } else {
      expectEqual(Leaf().method(), "AddInterposingMethod.method()")
      expectEqual(Leaf.classMethod(), "AddInterposingMethod.classMethod()")
    }
  }
}

SuperclassMethodsTest.test("RemoveInterposingMethod") {
  do {
    class Leaf : RemoveInterposingMethod {
      override func method() -> String {
        return super.method()
      }
      override class func classMethod() -> String {
        return super.classMethod()
      }
    }
    if getVersion() == 0 {
      expectEqual(Leaf().method(), "RemoveInterposingMethod.method()")
      expectEqual(Leaf.classMethod(), "RemoveInterposingMethod.classMethod()")
    } else {
      expectEqual(Leaf().method(), "Base.method()")
      expectEqual(Leaf.classMethod(), "Base.classMethod()")
    }
  }
}

SuperclassMethodsTest.test("InsertSuperclass") {
  do {
    class Leaf : InsertSuperclass {
      override func method() -> String {
        return super.method()
      }
      override class func classMethod() -> String {
        return super.classMethod()
      }
    }
    if getVersion() == 0 {
      expectEqual(Leaf().method(), "Base.method()")
      expectEqual(Leaf.classMethod(), "Base.classMethod()")
    } else {
      expectEqual(Leaf().method(), "InBetween.method()")
      expectEqual(Leaf.classMethod(), "InBetween.classMethod()")
    }
  }
}

SuperclassMethodsTest.test("ChangeRoot") {
  do {
    class Leaf : ChangeRoot {
      override func method() -> String {
        return super.method()
      }
      override class func classMethod() -> String {
        return super.classMethod()
      }
    }
    if getVersion() == 0 {
      expectEqual(Leaf().method(), "Base.method()")
      expectEqual(Leaf.classMethod(), "Base.classMethod()")
    } else {
      expectEqual(Leaf().method(), "OtherBase.method()")
      expectEqual(Leaf.classMethod(), "OtherBase.classMethod()")
    }
  }
}

runAllTests()

