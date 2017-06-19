// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | %FileCheck %s

// CHECK: <kw>let</kw> x = <str>"""
// CHECK-NEXT: This is an unterminated
// CHECK-NEXT: \( "multiline" )
// CHECK-NEXT: string followed by code
// CHECK-NEXT: ""
// CHECK-NEXT: func foo() {}
// CHECK-NEXT: </str>
let x = """
  This is an unterminated
  \( "multiline" )
  string followed by code
  ""
func foo() {}
