// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | %FileCheck %s

// CHECK: <#kw>#if</#kw> d
// CHECK-NEXT: <kw>func</kw> bar() {
// CHECK-NEXT: <#kw>#if</#kw> d
// CHECK-NEXT: }
// CHECK-NEXT: <kw>func</kw> foo() {}

#if d
func bar() {
  #if d
}
func foo() {}
