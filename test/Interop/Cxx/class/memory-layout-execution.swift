// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -o %t/class_layout -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/class_layout
// RUN: %target-run %t/class_layout 2&>1
//
// REQUIRES: executable_test

import MemoryLayout
import StdlibUnittest

var MemoryLayoutTestSuite = TestSuite("MemoryLayout")

MemoryLayoutTestSuite.test("PrivateMemberLayout") {
  // Check that, even though the private member variable 'a' is not imported, we
  // still use the same memory layout as Clang.
  expectEqual(sizeOfPrivateMemberLayout(),
    MemoryLayout<PrivateMemberLayout>.size)
  expectEqual(sizeOfPrivateMemberLayout(),
    MemoryLayout<PrivateMemberLayout>.stride)
  expectEqual(offsetOfPrivateMemberLayout_b(),
    MemoryLayout<PrivateMemberLayout>.offset(of: \PrivateMemberLayout.b));
}

runAllTests()
