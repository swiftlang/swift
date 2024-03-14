func foo() -> Int {
  #if true
  #if true
  return 1
  #else
  return 2
  #endif
  #else
  return 3
  #endif
}

// RUN: %sourcekitd-test -req=active-regions %s -- -D FLAG_1 -module-name active_regions %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: START IF CONFIGS
// CHECK1-NEXT: 2:3 - active
// CHECK1-NEXT: 3:3 - active
// CHECK1-NEXT: 5:3 - inactive
// CHECK1-NEXT: 8:3 - inactive
// CHECK1-NEXT: END IF CONFIGS
