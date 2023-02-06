func foo() {
  extension XXX {
// RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):9 %s -- %s
    var hex 
  }
}
