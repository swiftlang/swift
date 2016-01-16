// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/superclass_methods.swift -o %t/before/superclass_methods.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/superclass_methods.swift -o %t/before/superclass_methods.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/superclass_methods.swift -o %t/after/superclass_methods.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/superclass_methods.swift -o %t/after/superclass_methods.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/superclass_methods.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/superclass_methods.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/superclass_methods.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/superclass_methods.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

import StdlibUnittest
import superclass_methods

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

