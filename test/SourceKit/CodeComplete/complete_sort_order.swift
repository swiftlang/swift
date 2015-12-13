func foo(a a: String) {}
func foo(a a: Int) {}
func foo(b b: Int) {}

func test() {
  let x = 1

}

// RUN: %sourcekitd-test -req=complete -req-opts=hidelowpriority=0 -pos=7:1 %s -- %s > %t.orig
// RUN: FileCheck -check-prefix=NAME %s < %t.orig
// Make sure the order is as below, foo(Int) should come before foo(String).

// NAME: key.description: "__COLUMN__"
// NAME: key.description: "AbsoluteValuable"
// NAME: key.description: "foo(a: Int)"
// NAME-NOT: key.description
// NAME: key.description: "foo(a: String)"
// NAME-NOT: key.description
// NAME: key.description: "foo(b: Int)"
// NAME: key.description: "test()"
// NAME: key.description: "x"

// RUN: %sourcekitd-test -req=complete.open -pos=7:1 -req-opts=hidelowpriority=0,hideunderscores=0 %s -- %s > %t.default
// RUN: %sourcekitd-test -req=complete.open -pos=7:1 -req-opts=sort.byname=0,hidelowpriority=0,hideunderscores=0 %s -- %s > %t.on
// RUN: %sourcekitd-test -req=complete.open -pos=7:1 -req-opts=sort.byname=1,hidelowpriority=0,hideunderscores=0 %s -- %s > %t.off
// RUN: FileCheck -check-prefix=CONTEXT %s < %t.default
// RUN: FileCheck -check-prefix=NAME %s < %t.off
// FIXME: rdar://problem/20109989 non-deterministic sort order
// RUN-disabled: diff %t.on %t.default
// RUN: FileCheck -check-prefix=CONTEXT %s < %t.on

// CONTEXT: key.kind: source.lang.swift.decl
// CONTEXT-NEXT: key.name: "x"
// CONTEXT-NOT: key.name:
// CONTEXT: key.name: "foo(a:)"
// CONTEXT-NOT: key.name:
// CONTEXT: key.name: "foo(a:)"
// CONTEXT-NOT: key.name:
// CONTEXT: key.name: "foo(b:)"
// CONTEXT-NOT: key.name:
// CONTEXT: key.name: "test()"
// CONTEXT: key.name: "AbsoluteValuable"
// CONTEXT: key.name: "__COLUMN__"

// RUN: %complete-test -tok=STMT_0 %s | FileCheck %s -check-prefix=STMT
// RUN: %complete-test -tok=EXPR_0 %s | FileCheck %s -check-prefix=EXPR
func test1() {
  #^STMT_0^#
}
// STMT: let
// STMT: var
// STMT: if
// STMT: for
// STMT: while
// STMT: func
// STMT: foo(a: Int)

func test2() {
  (#^EXPR_0^#)
}
// EXPR: 0
// EXPR: 0.0
// EXPR: false
// EXPR: true
// EXPR: "text"
// EXPR: [item]
// EXPR: [key: value]
// EXPR: (item, item)
// EXPR: nil
// EXPR: [#Color(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float)#]
// EXPR: foo(a: Int)
