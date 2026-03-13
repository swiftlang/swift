// RUN: %sourcekitd-test -req=cursor -pos=6:7 %s -- %s | %FileCheck %s --check-prefix=CHECK-INT
// RUN: %sourcekitd-test -req=cursor -pos=8:7 %s -- %s | %FileCheck %s --check-prefix=CHECK-STR

func foo() {
#if USE_INT
  let xxx = 1
#else
  let xxx = "hello"
#endif
}
// CHECK-INT: <Declaration>let xxx: <Type usr="s:Si">Int</Type></Declaration>
// CHECK-STR: <Declaration>let xxx: <Type usr="s:SS">String</Type></Declaration>
