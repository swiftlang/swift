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
