// RUN: %empty-directory(%t)
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCEvilClassInitialization/EvilClass.m -c -o %t/EvilClass.o
// RUN: %target-build-swift -I %S/Inputs/ObjCEvilClassInitialization/ %t/EvilClass.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import EvilClass

import StdlibUnittest

let tests = TestSuite("ObjCEvilClassInitialization")

tests.test("GenericOnEvilClass") {
  if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    struct Generic<T> {
      var type: T.Type { return T.self }
    }
    let g = Generic<EvilClass>()
    expectEqual("\(type(of: g))", "Generic<EvilClass>")
    expectEqual(g.type, EvilClass.self)
  }
}

runAllTests()
