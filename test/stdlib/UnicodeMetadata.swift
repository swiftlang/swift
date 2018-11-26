// RUN: %target-build-swift -module-name="日本語01" %s -o %t.out
// RUN: %target-codesign %t.out
// RUN: %target-run %t.out | %FileCheck %s
// REQUIRES: executable_test


class myClass { }

// Check that the runtime doesn't crash when generating the class name with
// a non-ascii module name.
let array = [ myClass() ]

// CHECK: [日本語01.myClass]
print(array)
