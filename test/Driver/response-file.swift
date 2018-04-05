// RUN: echo "-DTEST" > %t.0.resp
// RUN: %swiftc_driver @%t.0.resp %s 2>&1 | %FileCheck %s -check-prefix=SHORT
// SHORT: warning: result of call to 'abs' is unused

// RUN: python -c 'for _ in range(200000): print "-DTEST"' > %t.1.resp
// RUN: %swiftc_driver @%t.1.resp %s 2>&1 | %FileCheck %s -check-prefix=LONG
// LONG: warning: result of call to 'abs' is unused

#if TEST
abs(-5)
#endif
