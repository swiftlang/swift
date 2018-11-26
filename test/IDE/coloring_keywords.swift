// We need to require macOS because swiftSyntax currently doesn't build on Linux
// REQUIRES: OS=macosx
// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | %FileCheck %s
// RUN: %swift-swiftsyntax-test -classify-syntax -source-file %s | %FileCheck %s

// CHECK: <kw>return</kw> c.return

class C {
  var `return` = 2
}

func foo(_ c: C) -> Int {
  return c.return
}
