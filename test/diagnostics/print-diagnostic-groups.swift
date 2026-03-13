// RUN: %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s --check-prefix=CHECK-FILE -check-prefix CHECK
// RUN: %target-swift-frontend -typecheck %s -diagnostic-documentation-path https://docs.swift.org/compiler/documentation/diagnostics/ 2>&1 | %FileCheck %s --check-prefix=CHECK-HTTPS -check-prefix CHECK
// REQUIRES: swift_swift_parser

// CHECK: warning: file 'print-diagnostic-groups.swift' is part of module 'main'; ignoring import{{$}}
import main

// This test checks that we get diagnostic groups as part of the printed output.

@available(*, deprecated, renamed: "bar2")
func bar() {
}

// CHECK: warning: 'bar()' is deprecated: renamed to 'bar2' [#DeprecatedDeclaration]{{$}}
bar()

// CHECK-FILE: [#DeprecatedDeclarations]: <file://{{.*}}deprecated-declaration.md>

// CHECK-HTTPS: [#DeprecatedDeclaration]: <https://docs.swift.org/compiler/documentation/diagnostics/deprecated-declaration>
