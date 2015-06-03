// RUN: %target-build-swift %s -o %t.out
// RUN: %target-run %t.out --stdlib-unittest-filter abc | FileCheck --check-prefix=CHECK-ABC %s
// RUN: %target-run %t.out --stdlib-unittest-filter xyz | FileCheck --check-prefix=CHECK-XYZ %s

// CHECK-ABC: StdlibUnittest: using filter: abc{{$}}
// CHECK-ABC: [ RUN      ] Filter.abc{{$}}
// CHECK-ABC: [       OK ] Filter.abc{{$}}
// CHECK-ABC: [ RUN      ] Filter.abcdef{{$}}
// CHECK-ABC: [       OK ] Filter.abcdef{{$}}
// CHECK-ABC-NOT: xyz

// CHECK-XYZ: StdlibUnittest: using filter: xyz{{$}}
// CHECK-XYZ-NOT: abc
// CHECK-XYZ: [ RUN      ] Filter.xyz{{$}}
// REQUIRES: executable_test
// CHECK-XYZ: [       OK ] Filter.xyz{{$}}
// CHECK-XYZ-NOT: abc
// CHECK-XYZ-NOT: xyz

import StdlibUnittest

var FilterTestSuite = TestSuite("Filter")

FilterTestSuite.test("abc") {
  expectEqual(1, 1)
}

FilterTestSuite.test("abcdef") {
  expectEqual(1, 1)
}

FilterTestSuite.test("xyz") {
  expectEqual(1, 1)
}

runAllTests()

