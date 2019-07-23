func foo() -> String {
  return "abc"
}

// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=localize-string -pos=2:10 %s -- %s > %t.result/localize-string.swift.expected
// RUN: diff -u %S/localize-string.swift.expected %t.result/localize-string.swift.expected

// REQUIRES-ANY: OS=macosx, OS=linux-gnu
