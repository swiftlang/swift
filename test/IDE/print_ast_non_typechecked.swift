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

// CHECK1-NOT: #if
