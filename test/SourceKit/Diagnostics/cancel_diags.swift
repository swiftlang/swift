// RUN: not %sourcekitd-test -req=diags %s -print-raw-response -id=diag -async -- %s == -cancel=diag 2>&1 | %FileCheck %s

func foo(x: Invalid1, y: Invalid2) {
    x / y / x / y / x / y / x / y
}

// CHECK: error response (Request Cancelled)
