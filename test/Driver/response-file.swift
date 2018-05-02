// RUN: echo "-DTEST0" > %t.0.resp
// RUN: %target-build-swift -typecheck @%t.0.resp %s 2>&1 | %FileCheck %s -check-prefix=SHORT
// SHORT: warning: result of call to 'abs' is unused

// RUN: python -c 'for a in ["A", "B", "C", "D"]: print "-DTEST1" + a' > %t.1.resp
// RUN: %target-build-swift -typecheck @%t.1.resp %s 2>&1 | %FileCheck %s -check-prefix=MULTIPLE
// MULTIPLE: warning: result of call to 'abs' is unused

// RUN: echo "-DTEST2A -DTEST2B" > %t.2.resp
// RUN: echo "-DTEST2C -DTEST2D" > %t.3.resp
// RUN: %target-build-swift -typecheck @%t.2.resp %s @%t.3.resp 2>&1 | %FileCheck %s -check-prefix=MIXED
// MIXED: warning: result of call to 'abs' is unused

// RUN: echo "-DTEST3A" > %t.4.resp
// RUN: echo "%s" >> %t.4.resp
// RUN: echo "-DTEST3B" >> %t.4.resp
// RUN: %target-build-swift -typecheck @%t.4.resp 2>&1 | %FileCheck %s -check-prefix=RESPONLY
// RESPONLY: warning: result of call to 'abs' is unused

// RUN: echo "-DTEST4A" > %t.5.resp
// RUN: echo "%s" >> %t.5.resp
// RUN: echo "@%t.5.resp" > %t.6.resp
// RUN: echo "-DTEST4B" >> %t.6.resp
// RUN: %target-build-swift -typecheck @%t.6.resp 2>&1 | %FileCheck %s -check-prefix=RECURSIVE
// RECURSIVE: warning: result of call to 'abs' is unused

// RUN: python -c 'for i in range(500001): print "-DTEST5_" + str(i)' > %t.7.resp
// RUN: %target-build-swift -typecheck @%t.7.resp %s 2>&1 | %FileCheck %s -check-prefix=LONG
// LONG: warning: result of call to 'abs' is unused

// RUN: python -c 'for i in range(500001): print "-DTEST6_" + str(i)' > %t.8.resp
// RUN: %empty-directory(%t/tmp)
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver %s @%t.8.resp -save-temps
// RUN: ls %t/tmp/arguments-*.resp

// RUN: python -c 'for i in range(500001): print "-DTEST7_" + str(i)' > %t.9.resp
// RUN: not %target-swiftc_driver %s @%t.9.resp -Xfrontend -debug-crash-immediately 2>&1 | %FileCheck %s -check-prefix=CRASH
// CRASH: @{{[^ ]*}}arguments-{{[0-9a-zA-Z]+}}.resp

#if TEST0
abs(-5)
#endif

#if TEST1A && TEST1B && TEST1C && TEST1D
abs(-5)
#endif

#if TEST2A && TEST2B && TEST2C && TEST2D
abs(-5)
#endif

#if TEST3A && TEST3B
abs(-5)
#endif

#if TEST4A && TEST4B
abs(-5)
#endif

#if TEST5_0 && TEST5_10 && TEST5_100 && TEST5_1000 && TEST5_10000 && TEST5_100000 && TEST5_500000
abs(-5)
#endif
