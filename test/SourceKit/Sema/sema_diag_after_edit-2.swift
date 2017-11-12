// RUN: %sourcekitd-test -req=open %s -- %s == -req=print-diags %s \
// RUN:    == -req=edit -pos=7:1 -replace="" -length=1 %s == -req=print-diags %s \
// RUN: | %FileCheck -check-prefix=CHECK-DIAG %s
// CHECK-DIAG-NOT: key.offset

class MyClass {
}

func foo() {
  let _ = "Hello, playground"
  let _ = MyClass()
}


