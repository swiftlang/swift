// RUN: %empty-directory(%t)
// RUN: %sourcekitd-test -req=index-to-store %s -index-store-path %t/idx -index-unit-output-path %t/index_locals.o -req-opts=include_locals=1 -- %s
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s


func foo(a: Int, b: Double) {
    var locVar = 1
}


// CHECK: variable(local)/Swift | locVar | s:12index_locals3foo1a1bySi_SdtF6locVarL_Sivp | <no-cgname> | Def,RelChild - RelChild,RelAcc
// CHECK-NEXT: function/acc-set(local)/Swift | setter:locVar | s:12index_locals3foo1a1bySi_SdtF6locVarL_Sivs | <no-cgname> | Def,Impl,RelChild,RelAcc -
