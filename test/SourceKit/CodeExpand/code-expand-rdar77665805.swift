enum E { case foo, bar }
func foo(x: (E) -> Void) {}
func test() {
  foo(x: <#T##(E) -> Void#>)
}

// RUN: %sourcekitd-test \
// RUN:   -req=open %s -- %s == \
// RUN:   -req=edit -offset=0 -length=53 -replace="" -req-opts=enablesyntaxmap=0,enablesubstructure=0,enablediagnostics=0 %s -- %s == \
// RUN:   -req=expand-placeholder -offset=23 -length=18 %s \
// RUN: | %FileCheck %s

// CHECK: {
// CHECK:   key.offset: 19,
// CHECK:   key.length: 23,
// CHECK:   key.sourcetext: " { <#E#> in\n<#code#>\n}"
// CHECK: }

