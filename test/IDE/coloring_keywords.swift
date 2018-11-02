// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -force-libsyntax-based-processing -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | %FileCheck %s

// CHECK: <kw>return</kw> c.return

class C {
  var `return` = 2
}

func foo(_ c: C) -> Int {
  return c.return
}
