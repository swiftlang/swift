// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macro.swift

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -external-plugin-path %t#%swift-plugin-server

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json MyApp casFSRootID > %t/fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/fs.casid | %FileCheck %s --check-prefix=FS

// FS: MacroDefinition

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %target-swift-frontend-plain -diagnostic-style=swift \
// RUN:   -emit-module -o %t/Test.swiftmodule -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name MyApp -O \
// RUN:   -external-plugin-path %t#%swift-plugin-server \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift @%t/MyApp.cmd -serialize-diagnostics-path %t/Test.diag 2>&1 | %FileCheck -check-prefix CHECK-DIAG %s

// CHECK-DIAG:      macro expansion #myWarning:1:1: warning: 'testDeprecated()' is deprecated [#DeprecatedDeclaration]
// CHECK-DIAG-NEXT: `- TMP_DIR{{/|\\}}main.swift:8:36: note: expanded code originates here
// CHECK-DIAG-NEXT:  6 |
// CHECK-DIAG-NEXT:  7 | func testDiscardableStringify(x: Int) {
// CHECK-DIAG-NEXT:  8 |   #toMyWarning("this is a warning")
// CHECK-DIAG-NEXT:    |   `- note: in expansion of macro 'toMyWarning' here
// CHECK-DIAG-NEXT:    +--- macro expansion #toMyWarning -----------------------------------
// CHECK-DIAG-NEXT:    |1 | #myWarning("")
// CHECK-DIAG-NEXT:    |  | `- note: in expansion of macro 'myWarning' here
// CHECK-DIAG-NEXT:    |  +--- macro expansion #myWarning ----------------------------------
// CHECK-DIAG-NEXT:    |  |1 | testDeprecated()
// CHECK-DIAG-NEXT:    |  |  | `- warning: 'testDeprecated()' is deprecated [#DeprecatedDeclaration]
// CHECK-DIAG-NEXT:    |  +-----------------------------------------------------------------
// CHECK-DIAG-NEXT:    +--------------------------------------------------------------------
// CHECK-DIAG-NEXT:  9 |   #myWarning("this is a warning")
// CHECK-DIAG-NEXT: 10 | }

// CHECK-DIAG:      macro expansion #myWarning:1:1: warning: 'testDeprecated()' is deprecated [#DeprecatedDeclaration]
// CHECK-DIAG-NEXT: `- TMP_DIR{{/|\\}}main.swift:9:34: note: expanded code originates here
// CHECK-DIAG-NEXT:  7 | func testDiscardableStringify(x: Int) {
// CHECK-DIAG-NEXT:  8 |   #toMyWarning("this is a warning")
// CHECK-DIAG-NEXT:  9 |   #myWarning("this is a warning")
// CHECK-DIAG-NEXT:    |   `- note: in expansion of macro 'myWarning' here
// CHECK-DIAG-NEXT:    +--- macro expansion #myWarning -------------------------------------
// CHECK-DIAG-NEXT:    |1 | testDeprecated()
// CHECK-DIAG-NEXT:    |  | `- warning: 'testDeprecated()' is deprecated [#DeprecatedDeclaration]
// CHECK-DIAG-NEXT:    +--------------------------------------------------------------------
// CHECK-DIAG-NEXT: 10 | }
// CHECK-DIAG-NEXT: 11 |
// CHECK-DIAG:      [#DeprecatedDeclaration]: <https://docs.swift.org/compiler/documentation/diagnostics/deprecated-declaration>

//--- macro.swift
import SwiftDiagnostics
import SwiftOperators
import SwiftSyntax
import SwiftSyntaxBuilder
@_spi(ExperimentalLanguageFeature) import SwiftSyntaxMacros

public struct CallDeprecatedMacro: ExpressionMacro {
   public static func expansion(
     of macro: some FreestandingMacroExpansionSyntax,
     in context: some MacroExpansionContext
   ) throws -> ExprSyntax {
     return "testDeprecated()"
  }
}

public struct ToMyWarningMacro: ExpressionMacro {
   public static func expansion(
     of macro: some FreestandingMacroExpansionSyntax,
     in context: some MacroExpansionContext
   ) throws -> ExprSyntax {
     return "#myWarning(\"\")"
  }
}

//--- main.swift
@freestanding(expression) macro myWarning(_ message: String) = #externalMacro(module: "MacroDefinition", type: "CallDeprecatedMacro")
@freestanding(expression) macro toMyWarning(_ message: String) = #externalMacro(module: "MacroDefinition", type: "ToMyWarningMacro")

@available(*, deprecated)
func testDeprecated() {}

func testDiscardableStringify(x: Int) {
  #toMyWarning("this is a warning")
  #myWarning("this is a warning")
}
