class MyClass {
  init(abc:Int) {
  }
}

class MySubClass: MyClass {
}

func foo() {
  MyClass(abc:1)
  MySubClass(abc:1)
}

// RUN: %sourcekitd-test -req=cursor -pos=10:7 %s -- %s | %FileCheck -check-prefix=CHECK1-INIT %s
// RUN: %sourcekitd-test -req=cursor -pos=10:12 %s -- %s | %FileCheck -check-prefix=CHECK1-PAR %s
// CHECK1-INIT: <Declaration>init(abc: <Type usr="s:Si">Int</Type>)</Declaration>
// CHECK1-PAR: <Declaration>let abc: <Type usr="s:Si">Int</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=11:8 %s -- %s | %FileCheck -check-prefix=CHECK2-INIT %s
// RUN: %sourcekitd-test -req=cursor -pos=11:16 %s -- %s | %FileCheck -check-prefix=CHECK2-PAR %s
// CHECK2-INIT: <Declaration>init(abc: <Type usr="s:Si">Int</Type>)</Declaration>
// CHECK2-PAR: <Declaration>let abc: <Type usr="s:Si">Int</Type></Declaration>
