// RUN: not %swift -fake-argument -abcdef -c %s -o %t.o 2>&1 | %FileCheck %s

// CHECK: <unknown>:0: error: unknown argument: '-fake-argument'
// CHECK-NEXT: <unknown>:0: error: unknown argument: '-abcdef'

// RUN: not %swiftc_driver -c %s -o %t.o -Xfrontend -fake-frontend-arg -Xfrontend fakevalue 2>&1 | %FileCheck -check-prefix=XFRONTEND %s

// XFRONTEND: <unknown>:0: error: unknown argument: '-fake-frontend-arg'

// RUN: not %swiftc_driver -D Correct -DAlsoCorrect -D@#%! -D Swift=Cool -D-D -c %s -o %t.o 2>&1 | %FileCheck -check-prefix=INVALID-COND %s
// INVALID-COND: <unknown>:0: error: conditional compilation flags must be valid Swift identifiers (rather than '@#%!')
// INVALID-COND-NEXT: <unknown>:0: warning: conditional compilation flags do not have values in Swift; they are either present or absent (rather than 'Swift=Cool')
// INVALID-COND-NEXT: <unknown>:0: error: invalid argument '-D-D'; did you provide a redundant '-D' in your build settings?

