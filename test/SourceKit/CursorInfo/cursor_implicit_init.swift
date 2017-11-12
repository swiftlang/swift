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

// RUN: %sourcekitd-test -req=cursor -pos=10:12 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: <Declaration>init(abc: <Type usr="s:Si">Int</Type>)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=11:16 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: <Declaration>init(abc: <Type usr="s:Si">Int</Type>)</Declaration>
