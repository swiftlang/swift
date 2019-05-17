// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_struct)) -enable-library-evolution %S/../../test/Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/%target-library-name(resilient_struct)

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -o %t/main %target-rpath(%t) -target %target-stable-abi-triple
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(resilient_struct) %t/libresilient_class%{target-shared-library-suffix}


// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

// Test that on an old Objective-C runtime we crash if attempting to use a
// class that requires the update callback and does not have a fragile layout.

import StdlibUnittest
import resilient_struct

var ClassTestSuite = TestSuite("Update callback")

public class ClassWithResilientField {
  var x = ResilientInt(i: 0)
}

@_optimize(none) func blackHole<T>(_: T) {}

@_optimize(none) func forceMetadata() {
  blackHole(ClassWithResilientField())
}

if #available(macOS 10.14.4, iOS 12.2, tvOS 12.2, watchOS 5.2, *) {
  ClassTestSuite.test("RealizeResilientClass") {  
    print("Nothing to test. We have the new Objective-C runtime.")
    forceMetadata()
  }
} else {
  ClassTestSuite.test("RealizeResilientClass")
    .crashOutputMatches("class ClassWithResilientField does not have a fragile layout")
    .code {
      expectCrashLater()
      print("About to crash...")
      forceMetadata()
    }
}

runAllTests()
