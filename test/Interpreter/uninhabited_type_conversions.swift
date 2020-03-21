// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -DT1 -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %{python} %S/../Inputs/not.py "%target-run %t/a.out" 2>&1 | %FileCheck %s --check-prefix CHECK1

// RUN: %target-build-swift %s -DT2 -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %{python} %S/../Inputs/not.py "%target-run %t/a.out" 2>&1 | %FileCheck %s --check-prefix CHECK2

// RUN: %target-build-swift %s -DT3 -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %{python} %S/../Inputs/not.py "%target-run %t/a.out" 2>&1 | %FileCheck %s --check-prefix CHECK3

// NOTE: not.py is used above instead of "not --crash" because %target-run
// doesn't pass through the crash, and `not` may not be available when running
// on a remote host.

// REQUIRES: executable_test

#if T1
fatalError("T1")
// CHECK1: Fatal error: T1
#endif

#if T2
let y: Int? = nil
var x = y ?? fatalError("T2")
x += 1
// CHECK2: Fatal error: T2
#endif

#if T3
let x: Int = fatalError("T3") + fatalError("Not this one")
// CHECK3: Fatal error: T3
#endif
