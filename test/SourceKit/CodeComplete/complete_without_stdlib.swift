class Str {
  var value: Str
}

// rdar://problem/58663066
// Test a environment where stdlib is not found.
// Completion should return zero result.

// RUN: %empty-directory(%t/rsrc)
// RUN: %empty-directory(%t/sdk)

// RUN: not %sourcekitd-test \
// RUN:   -req=global-config -req-opts=completion_max_astcontext_reuse_count=0 \
// RUN:   -req=complete -pos=4:1 %s -- %s -resource-dir %t/rsrc -sdk %t/sdk 2>&1 | %FileCheck %s
// RUN: not %sourcekitd-test \
// RUN:   -req=complete -pos=4:1 %s -- %s -resource-dir %t/rsrc -sdk %t/sdk == \
// RUN:   -req=complete -pos=4:1 %s -- %s -resource-dir %t/rsrc -sdk %t/sdk 2>&1 | %FileCheck %s

// CHECK: error response (Request Failed): Loading the standard library failed
