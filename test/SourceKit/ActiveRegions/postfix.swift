struct Value {
  var prop1: Int = 0
  var prop2: Int = 0
}
func test(value: Value) {
  let _ = value
#if false
    .prop1
#else
    .prop2
#endif
}

// RUN: %sourcekitd-test -req=active-regions %s -- -module-name active_regions %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: START IF CONFIGS
// CHECK1-NEXT: 7:1 - inactive
// CHECK1-NEXT: 9:1 - active
// CHECK1-NEXT: END IF CONFIGS
