class C {
  func foo(s: Int, w
}

// RUN: %target-swift-ide-test -print-ast-not-typechecked -source-filename %s | %FileCheck %s -check-prefix=CHECK1
// CHECK1: func foo(s: Int)

#if BAR
func bar() {}
#elseif BAZ
func baz() {}
#else
func qux() {}
#endif

// CHECK1: {{^}}#if /* condition */
// CHECK1: {{^}}  func bar() {
// CHECK1: {{^}}  }
// CHECK1: {{^}}#elseif /* condition */
// CHECK1: {{^}}  func baz() {
// CHECK1: {{^}}  }
// CHECK1: {{^}}#else
// CHECK1: {{^}}  func qux() {
// CHECK1: {{^}}  }
// CHECK1: {{^}}#endif
