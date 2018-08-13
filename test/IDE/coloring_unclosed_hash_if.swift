// We need to require macOS because swiftSyntax currently doesn't build on Linux
// REQUIRES: OS=macosx
// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | %FileCheck %s
// RUN: %swift-swiftsyntax-test -classify-syntax -source-file %s | %FileCheck %s

// CHECK: <#kw>#if</#kw> <#id>d</#id>
// CHECK-NEXT: <kw>func</kw> bar() {
// CHECK-NEXT: <#kw>#if</#kw> <#id>d</#id>
// CHECK-NEXT: }
// CHECK-NEXT: <kw>func</kw> foo() {}

#if d
func bar() {
  #if d
}
func foo() {}
