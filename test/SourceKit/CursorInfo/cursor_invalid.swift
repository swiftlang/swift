class C {
  func foo() {
    for child : AnyObject in childs! {
        let c : C
    }
  }
  var over under: Int
  var bad: IDontExist
  let var: Int
}

{
  class MyCoolClass: Undeclared {}
}

func resyncParser1() {}

func ==
func ==()
func ==(x: C)
func ==(x: C, y: C)

func resyncParser2() {}

// RUN: %sourcekitd-test -req=cursor -pos=4:13 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: source.lang.swift.decl.var.local (4:13-4:14)
// CHECK1: c
// CHECK1: <Declaration>let c</Declaration>
// CHECK1: OVERRIDES BEGIN
// CHECK1: OVERRIDES END

// RUN: %sourcekitd-test -req=cursor -pos=13:10 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: source.lang.swift.decl.class (13:9-13:20)
// CHECK2: MyCoolClass
// CHECK2: <Declaration>class MyCoolClass : Undeclared</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=7:7 %s -- %s | %FileCheck -check-prefix=CHECK3 %s
// CHECK3: source.lang.swift.decl.var.instance (7:7-7:11)
// CHECK3: over
// CHECK3: <Declaration>var over{{.*}}</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=8:7 %s -- %s | %FileCheck -check-prefix=CHECK4 %s
// CHECK4: source.lang.swift.decl.var.instance (8:7-8:10)
// CHECK4: bad
// CHECK4: <Declaration>var bad: IDontExist</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=7:12 %s -- %s | %FileCheck -check-prefix=EMPTY %s
// RUN: %sourcekitd-test -req=cursor -pos=9:7 %s -- %s | %FileCheck -check-prefix=EMPTY %s
// EMPTY: <empty cursor info>

// RUN: %sourcekitd-test -req=cursor -pos=18:6 %s -- %s | %FileCheck -check-prefix=EQEQ1 %s
// RUN: %sourcekitd-test -req=cursor -pos=19:6 %s -- %s | %FileCheck -check-prefix=EQEQ1 %s
// Note: we can't find the operator decl so the decl kind is a fallback.
// EQEQ1: <decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>==</decl.name>()

// RUN: %sourcekitd-test -req=cursor -pos=20:6 %s -- %s | %FileCheck -check-prefix=EQEQ2 %s
// Note: we can't find the operator decl so the decl kind is a fallback.
// EQEQ2: <decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>==</decl.name>(<decl.var.parameter><decl.var.parameter.name>x</decl.var.parameter.name>: <decl.var.parameter.type><ref.class usr="s:14cursor_invalid1CC">C</ref.class></decl.var.parameter.type></decl.var.parameter>)</decl.function.free>

// RUN: %sourcekitd-test -req=cursor -pos=21:6 %s -- %s | %FileCheck -check-prefix=EQEQ3 %s
// EQEQ3: <decl.function.operator.infix><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>==</decl.name>(<decl.var.parameter><decl.var.parameter.name>x</decl.var.parameter.name>: <decl.var.parameter.type><ref.class usr="s:14cursor_invalid1CC">C</ref.class></decl.var.parameter.type></decl.var.parameter>, <decl.var.parameter><decl.var.parameter.name>y</decl.var.parameter.name>: <decl.var.parameter.type><ref.class usr="s:14cursor_invalid1CC">C</ref.class></decl.var.parameter.type></decl.var.parameter>)</decl.function.operator.infix>
