// REQUIRES: shell

// RUN: %empty-directory(%t)
// RUN: env TMPDIR=%t __XPC_TMPDIR=%t %sourcekitd-test -req=syntax-map %s 
// RUN: ls %t/ | count 0

// RUN: %empty-directory(%t)
// RUN: env TMPDIR=%t __XPC_TMPDIR=%t %sourcekitd-test -req=syntax-map %s -- %s
// RUN: ls %t/ | count 0

// RUN: %empty-directory(%t)
// RUN: env TMPDIR=%t __XPC_TMPDIR=%t %sourcekitd-test -req=syntax-map %s -- %s -c -o %t/foo.o
// RUN: ls %t/ | count 0

// RUN: %empty-directory(%t)
// RUN: env TMPDIR=%t __XPC_TMPDIR=%t %sourcekitd-test -req=sema %s -- %s
// RUN: ls %t/ | count 0

// RUN: %empty-directory(%t)
// RUN: env TMPDIR=%t __XPC_TMPDIR=%t %sourcekitd-test -req=sema %s -- %s -c -o %t/foo.o
// RUN: ls %t/ | count 0

// RUN: %empty-directory(%t)
// RUN: env TMPDIR=%t __XPC_TMPDIR=%t %sourcekitd-test -req=sema %s -- %s -emit-module -module-name main -emit-module-path %t/main.swiftmodule -emit-module-interface -enable-library-evolution -emit-module-interface-path %t/main.swiftinterface -emit-objc-header -emit-objc-header-path %t/main.h -c -o %t/foo.o
// RUN: ls %t/ | count 0

// RUN: %empty-directory(%t)
// RUN: env TMPDIR=%t __XPC_TMPDIR=%t %sourcekitd-test -req=sema %s -- %s -emit-module -module-name main -emit-module-path %t/main.swiftmodule -emit-executable -o %t/foo
// RUN: ls %t/ | count 0
