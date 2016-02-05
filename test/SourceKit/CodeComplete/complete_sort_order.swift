func foo(a a: String) {}
func foo(a a: Int) {}
func foo(b b: Int) {}

func test() {
  let x = 1

}

// RUN: %sourcekitd-test -req=complete -req-opts=hidelowpriority=0 -pos=7:1 %s -- %s > %t.orig
// RUN: FileCheck -check-prefix=NAME %s < %t.orig
// Make sure the order is as below, foo(Int) should come before foo(String).

// NAME: key.description: "#column"
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
// CONTEXT: key.name: "#column"

// RUN: %complete-test -tok=STMT_0 %s | FileCheck %s -check-prefix=STMT
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

// RUN: %complete-test -top=0 -tok=EXPR_0 %s | FileCheck %s -check-prefix=EXPR
func test2() {
  (#^EXPR_0^#)
}
// EXPR: 0
// EXPR: "abc"
// EXPR: true
// EXPR: false
// EXPR: [#Color(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float)#]
// EXPR: [#Image(imageLiteral: String)#]
// EXPR: [values]
// EXPR: [key: value]
// EXPR: (values)
// EXPR: nil
// EXPR: foo(a: Int)

// Top 1
// RUN: %complete-test -top=1 -tok=EXPR_1 %s | FileCheck %s -check-prefix=EXPR_TOP_1
func test3(x: Int) {
  let y = x
  let z = x
  let zzz = x
  (#^EXPR_1^#)
}
// EXPR_TOP_1: x
// EXPR_TOP_1: 0
// EXPR_TOP_1: "abc"
// EXPR_TOP_1: true
// EXPR_TOP_1: false
// EXPR_TOP_1: [#Color(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float)#]
// EXPR_TOP_1: [#Image(imageLiteral: String)#]
// EXPR_TOP_1: [values]
// EXPR_TOP_1: [key: value]
// EXPR_TOP_1: (values)
// EXPR_TOP_1: nil
// EXPR_TOP_1: y
// EXPR_TOP_1: z
// EXPR_TOP_1: zzz

// Top 3
// RUN: %complete-test -top=3 -tok=EXPR_2 %s | FileCheck %s -check-prefix=EXPR_TOP_3
func test4(x: Int) {
  let y = x
  let z = x
  let zzz = x
  (#^EXPR_2^#)
}
// EXPR_TOP_3: x
// EXPR_TOP_3: y
// EXPR_TOP_3: z
// EXPR_TOP_3: 0
// EXPR_TOP_3: "abc"
// EXPR_TOP_3: true
// EXPR_TOP_3: false
// EXPR_TOP_3: [#Color(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float)#]
// EXPR_TOP_3: [#Image(imageLiteral: String)#]
// EXPR_TOP_3: [values]
// EXPR_TOP_3: [key: value]
// EXPR_TOP_3: (values)
// EXPR_TOP_3: nil
// EXPR_TOP_3: zzz

// Top 3 with type matching
// RUN: %complete-test -top=3 -tok=EXPR_3 %s | FileCheck %s -check-prefix=EXPR_TOP_3_TYPE_MATCH
func test4(x: Int) {
  let y: String = ""
  let z: String = y
  let zzz = x
  let bar: Int = #^EXPR_3^#
}
// EXPR_TOP_3_TYPE_MATCH: x
// EXPR_TOP_3_TYPE_MATCH: zzz
// EXPR_TOP_3_TYPE_MATCH: 0
// EXPR_TOP_3_TYPE_MATCH: y
// EXPR_TOP_3_TYPE_MATCH: z
