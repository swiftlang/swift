// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/print_synthesized_extensions_nomerge.swiftmodule -emit-module-doc -emit-module-doc-path %t/print_synthesized_extensions.swiftdoc %s
// RUN: %target-swift-ide-test -print-module -annotate-print -synthesize-extension -print-interface -no-empty-line-between-members -module-to-print=print_synthesized_extensions_nomerge -I %t -source-filename=%s > %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK1 < %t.syn.txt

public struct S1 {}

@available(macOS 10.15, *)
public extension S1 {
  func foo() {}
}

@available(iOS 13, *)
public extension S1 {
  func bar() {}
}

// CHECK1: <decl:Extension>@available(OSX 10.15, *)
// CHECK1:  extension <loc><ref:Struct>S1</ref></loc> {
// CHECK1: <decl:Extension>@available(iOS 13, *)
// CHECK1: extension <loc><ref:Struct>S1</ref></loc> {
