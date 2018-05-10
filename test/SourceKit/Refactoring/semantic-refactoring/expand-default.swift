enum E {
  case e1
  case e2
}
func foo(e : E) {
  switch (e) {
  default:
    return
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=expand-default -pos=7:7 %s -- %s > %t.result/expand-default.swift.expected
// RUN: diff -u %S/expand-default.swift.expected %t.result/expand-default.swift.expected

// REQUIRES-ANY: OS=macosx, OS=linux-gnu
