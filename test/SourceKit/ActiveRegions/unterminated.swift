#if FLAG_1
class Foo {
}
#else
class Fallback {
}

// RUN: %sourcekitd-test -req=active-regions %s -- -D FLAG_1 -module-name active_regions %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: START IF CONFIGS
// CHECK1-NEXT: 1:1 - active
// CHECK1-NEXT: 4:1 - inactive
// CHECK1-NEXT: END IF CONFIGS
