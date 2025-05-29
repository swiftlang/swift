// RUN: echo "-DTEST0" > %t.0.resp
// RUN: %target-build-swift -typecheck @%t.0.resp %s 2>&1 | %FileCheck %s -check-prefix=SHORT
// SHORT: warning: result of call to 'abs' is unused

// RUN: %{python} -c 'for a in ["A", "B", "C", "D"]: print("-DTEST1" + a)' > %t.1.resp
// RUN: %target-build-swift -typecheck @%t.1.resp %s 2>&1 | %FileCheck %s -check-prefix=MULTIPLE
// MULTIPLE: warning: result of call to 'abs' is unused

// RUN: echo "-DTEST2A -DTEST2B" > %t.2A.resp
// RUN: echo "-DTEST2C -DTEST2D" > %t.2B.resp
// RUN: %target-build-swift -typecheck @%t.2A.resp %s @%t.2B.resp 2>&1 | %FileCheck %s -check-prefix=MIXED
// MIXED: warning: result of call to 'abs' is unused

// RUN: echo "-DTEST3A" > %t.3.resp
// RUN: echo "%s" >> %t.3.resp
// RUN: echo "-DTEST3B" >> %t.3.resp
// RUN: %target-build-swift -typecheck @%t.3.resp 2>&1 | %FileCheck %s -check-prefix=RESPONLY
// RESPONLY: warning: result of call to 'abs' is unused

// RUN: echo "-DTEST4A" > %t.4A.resp
// RUN: echo "%s" >> %t.4A.resp
// RUN: echo "@%t.4A.resp" > %t.4B.resp
// RUN: echo "-DTEST4B" >> %t.4B.resp
// RUN: %target-build-swift -typecheck @%t.4B.resp 2>&1 | %FileCheck %s -check-prefix=RECURSIVE
// RECURSIVE: warning: result of call to 'abs' is unused

// RUN: %{python} -c 'for i in range(500001): print("-DTEST5_" + str(i))' > %t.5.resp
// RUN: %target-build-swift -typecheck @%t.5.resp %s 2>&1 | %FileCheck %s -check-prefix=LONG
// LONG: warning: result of call to 'abs' is unused
// RUN: %empty-directory(%t/tmp)
// RUN: env TMPDIR=%t/tmp/ TMP=%t/tmp/ %target-swiftc_driver %s @%t.5.resp -save-temps -o %t/main
// RUN: ls %t/tmp | grep arguments.*resp
// RUN: %target-build-swift -typecheck -v @%t.5.resp %s 2>&1 | %FileCheck %s -check-prefix=VERBOSE
// VERBOSE: @{{[^ ]*}}arguments-{{[0-9a-zA-Z]+}}.resp{{"?}} # -frontend -typecheck -primary-file
// RUN: not %target-swiftc_driver %s @%t.5.resp -Xfrontend -debug-crash-immediately 2>&1 | %FileCheck %s -check-prefix=TRACE
// TRACE: Program arguments: {{[^ ]*}}swift{{(c|c-legacy-driver|-frontend)?(\.exe)?}} -frontend -c -primary-file


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
