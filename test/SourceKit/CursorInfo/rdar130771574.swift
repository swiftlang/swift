struct S {
  var x: Int
}

// Make sure we can still resolve 'x' even with the trailing dot.
func foo(_ s: S) {
  s.x.
  // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):5 %s -- %s -module-name main | %FileCheck %s
}
// CHECK: s:4main1SV1xSivp
