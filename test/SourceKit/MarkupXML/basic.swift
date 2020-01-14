// RUN: %sourcekitd-test -req=markup-xml -pass-as-sourcetext %S/Input/DocComment1.md > %t.DocComment1.response
// RUN: diff --strip-trailing-cr -u %s.DocComment1.response %t.DocComment1.response
