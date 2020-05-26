// RUN: %sourcekitd-test -req=index %s -- -Xfrontend -serialize-diagnostics-path -Xfrontend %t.dia %s | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response

protocol P {}
class C {
extension Int: P {}
