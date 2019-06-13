let foo: Int = 10
foo

// Checks that the SourceKit request succeeded.
// CHECK-SOURCEKIT: source.lang.swift.ref.var.global (1:5-1:8)

// Checks that nothing has been written into the module cache on the real
// filesystem.
// CHECK-LS-NOT: ModuleCache

// RUN: %empty-directory(%t)
// RUN: %sourcekitd-test -in-memory-clang-module-cache -req=cursor -pos=2:1 %s -- %s -module-cache-path %t/ModuleCache | %FileCheck --check-prefix=CHECK-SOURCEKIT %s
// RUN: ls -l %t | %FileCheck --check-prefix=CHECK-LS %s
