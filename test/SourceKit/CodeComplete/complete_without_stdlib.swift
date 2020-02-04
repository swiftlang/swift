class Str {
  var value: Str
}

// rdar://problem/58663066
// Test a environment where stdlib is not found.
// Completion should return zero result.

// RUN: %empty-directory(%t/rsrc)
// RUN: %empty-directory(%t/sdk)

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=4:1 %s -- %s -resource-dir %t/rsrc -sdk %t/sdk | %FileCheck %s
// RUN: %sourcekitd-test \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=4:1 %s -- %s -resource-dir %t/rsrc -sdk %t/sdk == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=4:1 %s -- %s -resource-dir %t/rsrc -sdk %t/sdk | %FileCheck %s

// CHECK: key.results: [
// CHECK-NOT: key.description:
