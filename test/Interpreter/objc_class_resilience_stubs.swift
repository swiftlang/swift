// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_struct)) -enable-library-evolution %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule
// RUN: %target-codesign %t/%target-library-name(resilient_struct)

// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_objc_class)) -I %t -L %t -lresilient_struct -enable-library-evolution %S/../Inputs/resilient_objc_class.swift -emit-module -emit-module-path %t/resilient_objc_class.swiftmodule
// RUN: %target-codesign %t/%target-library-name(resilient_objc_class)

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -lresilient_objc_class -o %t/main %target-rpath(%t) -target %target-next-stable-abi-triple
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(resilient_struct) %t/%target-library-name(resilient_objc_class)

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_stable_abi

import StdlibUnittest
import Foundation
import resilient_objc_class

// Old OS versions do not have this hook.
let loadClassrefMissing = {
  nil == dlsym(UnsafeMutableRawPointer(bitPattern: -2),
               "objc_loadClassref")
}()

var ResilientClassTestSuite = TestSuite("ResilientClass")

class ResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}

@_optimize(none) func blackHole<T>(_: T) {}

@_optimize(none) func forceMetadata() {
  blackHole(ResilientNSObjectSubclass())
}

// This should not crash on older runtimes because we check before
// attempting to register the class stub.
ResilientClassTestSuite.test("RealizeResilientClass")
  .xfail(.osxMinor(10, 10, reason:
         "Fails on 10.9 and 10.10 -- rdar://51036773"))
  .code {
  forceMetadata()
}

@objc protocol MyProtocol {
  func myMethod() -> Int
}

extension ResilientNSObjectSubclass : MyProtocol {
  @objc func myMethod() -> Int { return 42 }
}

ResilientClassTestSuite.test("category on my class")
  .skip(.custom({ loadClassrefMissing },
                reason: "class stubs support not present"))
  .code {
  print(ResilientNSObjectSubclass.self)
  let o = ResilientNSObjectSubclass()
  expectEqual(42, (o as MyProtocol).myMethod())
}

@objc protocol AnotherProtocol {
  func anotherMethod() -> Int
}

extension ResilientNSObjectOutsideParent : AnotherProtocol {
  @objc func anotherMethod() -> Int { return 69 }
}

ResilientClassTestSuite.test("category on other class")
  .skip(.custom({ loadClassrefMissing },
                reason: "class stubs support not present"))
  .code {
  let o = ResilientNSObjectOutsideParent()
  expectEqual(69, (o as AnotherProtocol).anotherMethod())
}

runAllTests()
