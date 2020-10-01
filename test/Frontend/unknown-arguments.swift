// RUN: not %swift -fake-argument -abcdef -c %s -o %t.o 2>&1 | %FileCheck %s

// CHECK: error: unknown argument: '-fake-argument'
// CHECK-NEXT: error: unknown argument: '-abcdef'

// RUN: not %swiftc_driver -c %s -o %t.o -Xfrontend -fake-frontend-arg -Xfrontend fakevalue 2>&1 | %FileCheck -check-prefix=XFRONTEND %s

// XFRONTEND: error: unknown argument: '-fake-frontend-arg'

// RUN: not %swiftc_driver -D Correct -DAlsoCorrect -D@#%! -D Swift=Cool -D-D -c %s -o %t.o 2>&1 | %FileCheck -check-prefix=INVALID-COND %s
// INVALID-COND: error: conditional compilation flags must be valid Swift identifiers (rather than '@#%!')
// INVALID-COND-NEXT: warning: conditional compilation flags do not have values in Swift; they are either present or absent (rather than 'Swift=Cool')
// INVALID-COND-NEXT: error: invalid argument '-D-D'; did you provide a redundant '-D' in your build settings?

