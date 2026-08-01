// RUN: %target-build-swift %s -o %t.out
// RUN: %target-codesign %t.out
// RUN: not --crash %target-run %t.out 2>&1 | %FileCheck %s
// REQUIRES: executable_test

import StdlibUnittest

let DuplicateTestName = TestSuite("DuplicateTestName")

DuplicateTestName.test("duplicate") {}
DuplicateTestName.test("duplicate") {}

// CHECK: Fatal error: test 'duplicate' already exists in test suite 'DuplicateTestName'
