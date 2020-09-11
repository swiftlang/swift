// RUN: %sourcekitd-test -req=sema %s -- %s -enforce-exclusivity=checked | %FileCheck %s -check-prefix=CHECKED
// RUN: %sourcekitd-test -req=sema %s -- %s -enforce-exclusivity=none | %FileCheck %s -check-prefix=NONE
// CHECKED: modification requires exclusive access
// NONE-NOT: modification requires exclusive access

struct X {
  var f = 7
}

func takesInout<T>(_ p1: inout T, _ p2: inout T) {}

func hasStaticViolation() {
  var l = X()
  takesInout(&l.f, &l.f)
}
