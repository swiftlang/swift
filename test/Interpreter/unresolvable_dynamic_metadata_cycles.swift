// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/libresil.%target-dylib-extension) -Xfrontend -enable-resilience %S/Inputs/resilient_generic_struct_v1.swift -emit-module -emit-module-path %t/resil.swiftmodule -module-name resil
// RUN: %target-codesign %t/libresil.%target-dylib-extension

// RUN: %target-build-swift %s -L %t -I %t -lresil -o %t/main -Xlinker -rpath -Xlinker %t
// RUN: %target-codesign %t/main

// RUN: %target-build-swift-dylib(%t/libresil.%target-dylib-extension) -Xfrontend -enable-resilience %S/Inputs/resilient_generic_struct_v2.swift -emit-module -emit-module-path %t/resil.swiftmodule -module-name resil
// RUN: %target-codesign %t/libresil.%target-dylib-extension

// RUN: %target-run %t/main %t/libresil.%target-dylib-extension

// REQUIRES: executable_test

import StdlibUnittest

// We build this code against a version of 'resil' where
// ResilientGenericStruct<T> doesn't store a T, then switch the
// dynamic library to a new version where it does, introducing
// an unresolvable dynamic cycle.
//
// It would also be sufficient to demonstrate this crash if the
// compiler *actually* didn't know about the internal implementation
// details of 'resil' when building this file, but since it currently
// still does, it'll report a cycle immediately if we don't pull
// this switcharoo.
import resil

var DynamicMetadataCycleTests =
  TestSuite("Unresolvable dynamic metadata cycle tests")

enum test0_Node {
    case link(ResilientGenericStruct<test0_Node>)
}


DynamicMetadataCycleTests.test("cycle through enum")
  .crashOutputMatches("runtime error: unresolvable type metadata dependency cycle detected")
  .crashOutputMatches("  main.test0_Node")
  .crashOutputMatches("  depends on layout of resil.ResilientGenericStruct<main.test0_Node")
  .crashOutputMatches("  depends on layout of main.test0_Node")
  .code {
    expectCrashLater()
    _blackHole(test0_Node.self)
  }

runAllTests()
