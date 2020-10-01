// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name=_Test -import-objc-header %S/Inputs/check_class_for_archiving.h -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

class SwiftClass {}

class ObjcClass : NSObject {}

private class PrivateClass : NSObject {}

@objc(named_class)
private class NamedClass1 : NSObject {}

@objc(_T3nix11NamedClass2C)
private class NamedClass2 : NSObject {}

class GenericClass<T> : NSObject {}

class DerivedClass : GenericClass<Int> {}

@objc(_T3nix20DerivedClassWithNameC)
private class DerivedClassWithName : GenericClass<Int> {}

struct ABC {
  class InnerClass : NSObject {}
}

struct DEF<T> {
  class InnerClass : NSObject {}
}

let suite = TestSuite("check_class_for_archiving")
defer { runAllTests() }

let op: Int32 = 0 // archiving

suite.test("SwiftClass") {
  expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(SwiftClass.self, operation: op))
}
suite.test("ObjcClass") {
  expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(ObjcClass.self, operation: op))
}
suite.test("NamedClass1") {
  expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(NamedClass1.self, operation: op))
}
suite.test("NamedClass2") {
  expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(NamedClass2.self, operation: op))
}
suite.test("DerivedClass") {
  expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(DerivedClass.self, operation: op))
}
suite.test("DerivedClassWithName") {
  expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(DerivedClassWithName.self, operation: op))
}
suite.test("NSKeyedUnarchiver") {
  expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(NSKeyedUnarchiver.self, operation: op))
}

// Disable negative tests on older OSes because of rdar://problem/50504765
if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  suite.test("PrivateClass") {
    expectNotEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(PrivateClass.self, operation: op))
  }
}

if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) {
  // Generic classes and nested classes were considered to have unstable names
  // in earlier releases.
  suite.test("GenericClass") {
    expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(GenericClass<Int>.self, operation: op))
  }
  suite.test("InnerClass") {
    print(NSStringFromClass(ABC.InnerClass.self))
    expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(ABC.InnerClass.self, operation: op))
  }
  suite.test("InnerClass2") {
    print(NSStringFromClass(DEF<Int>.InnerClass.self))
    expectEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(DEF<Int>.InnerClass.self, operation: op))
  }

  suite.test("LocalClass") {
    class LocalClass: NSObject {}
    expectNotEqual(0, NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(LocalClass.self, operation: op))
  }
}
