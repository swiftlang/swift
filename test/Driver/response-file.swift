// RUN: echo "-DTEST" > %t.0.resp
// RUN: %swiftc_driver @%t.0.resp %s 2>&1 | %FileCheck %s -check-prefix=SHORT
// SHORT: warning: result of call to 'abs' is unused

// RUN: python -c 'for i in range(10000): print "-DTEST" + str(i)' > %t.1.resp
// RUN: %swiftc_driver -DTEST @%t.1.resp %s 2>&1 | %FileCheck %s -check-prefix=LONG
// LONG: warning: result of call to 'abs' is unused

// RUN: echo "-DTEST -DTESTB" > %t.2.resp
// RUN: echo "-DTESTC -DTESTD" > %t.3.resp
// RUN: %swiftc_driver @%t.2.resp %s @%t.3.resp 2>&1 | %FileCheck %s -check-prefix=MIXED
// MIXED: warning: result of call to 'abs' is unused

// RUN: echo "-DTEST" > %t.4.resp
// RUN: echo "%s" >> %t.4.resp
// RUN: echo "-DTEST1" >> %t.4.resp
// RUN: %swiftc_driver @%t.4.resp 2>&1 | %FileCheck %s -check-prefix=RESPONLY
// RESPONLY: warning: result of call to 'abs' is unused

// RUN: echo "-DTEST" > %t.5.resp
// RUN: echo "%s" >> %t.5.resp
// RUN: echo "@%t.5.resp" > %t.6.resp
// RUN: echo "-DTEST1" >> %t.6.resp
// RUN: %swiftc_driver @%t.6.resp 2>&1 | %FileCheck %s -check-prefix=RECURSIVE
// RECURSIVE: warning: result of call to 'abs' is unused

#if TEST
abs(-5)
#endif
