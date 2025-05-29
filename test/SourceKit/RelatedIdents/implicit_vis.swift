// RUN: %empty-directory(%t)
// RUN: split-file %s %t

//--- a.swift

class A {
  var b: B
  var c: B
}

//--- b.swift

class B {
  var a: A
}

//--- dummy.swift

// REQUIRES: swift_swift_parser
// RUN: %sourcekitd-test -req=related-idents -pos=3:10 %t/a.swift -- %t/a.swift %t/b.swift -o implicit_vis.o | %FileCheck -check-prefix=CHECK1 %s

// CHECK1: START RANGES
// CHECK1: 3:10 - 1
// CHECK1: 4:10 - 1
// CHECK1: END RANGES
