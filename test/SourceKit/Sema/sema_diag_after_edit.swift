let s : String = "f

// RUN: %sourcekitd-test -req=open %s -- %s == \
// RUN:    -req=print-diags %s | %FileCheck -check-prefix=CHECK-DIAG %s
// CHECK-DIAG: key.severity: source.diagnostic.severity.error

// Make sure that the diagnostic does not 'linger' after the edit is made.
// RUN: %sourcekitd-test -req=open %s -- %s == -req=edit -pos=1:20 -replace="\"" -length=0 %s == \
// RUN:    -req=print-diags %s > %t
// RUN: %FileCheck -check-prefix=CHECK-NODIAG %s < %t
// CHECK-NODIAG-NOT: key.severity: source.diagnostic.severity.error
