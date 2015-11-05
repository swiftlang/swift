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

// RUN: %sourcekitd-test -req=cursor -pos=4:13 %s -- %s | FileCheck -check-prefix=CHECK1 %s
// CHECK1: source.lang.swift.decl.var.local (4:13-4:14)
// CHECK1: c
// CHECK1: <Declaration>let c</Declaration>
// CHECK1: OVERRIDES BEGIN
// CHECK1: OVERRIDES END

// RUN: %sourcekitd-test -req=cursor -pos=13:10 %s -- %s | FileCheck -check-prefix=CHECK2 %s
// CHECK2: source.lang.swift.decl.class (13:9-13:20)
// CHECK2: MyCoolClass
// CHECK2: <Declaration>class MyCoolClass : Undeclared</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=7:7 %s -- %s | FileCheck -check-prefix=CHECK3 %s
// CHECK3: source.lang.swift.decl.var.instance (7:7-7:11)
// CHECK3: over
// CHECK3: <Declaration>var over{{.*}}</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=8:7 %s -- %s | FileCheck -check-prefix=CHECK4 %s
// CHECK4: source.lang.swift.decl.var.instance (8:7-8:10)
// CHECK4: bad
// CHECK4: <Declaration>var bad: &lt;&lt;error type&gt;&gt;</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=7:12 %s -- %s | FileCheck -check-prefix=EMPTY %s
// RUN: %sourcekitd-test -req=cursor -pos=9:7 %s -- %s | FileCheck -check-prefix=EMPTY %s
// EMPTY: <empty cursor info>
