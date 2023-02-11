// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-experimental-feature Macros -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -disable-availability-checking
// RUN: %target-swift-frontend -swift-version 5 -typecheck -enable-experimental-feature Macros -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -disable-availability-checking -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DUMP %s < %t/expansions-dump.txt


// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@attached(peer)
macro addCompletionHandler() = #externalMacro(module: "MacroDefinition", type: "AddCompletionHandler")

struct S {
  @addCompletionHandler
  func f(a: Int, for b: String, _ value: Double) async -> String {
    return b
  }

  // CHECK-DUMP: @__swiftmacro_18macro_expand_peers1SV1f1a3for_SSSi_SSSdtYaF20addCompletionHandlerfMp_.swift
  // CHECK-DUMP: func f(a: Int, for b: String, _ value: Double, completionHandler: (String) -> Void) {
  // CHECK-DUMP:   Task {
  // CHECK-DUMP:     completionHandler(await f(a: a, for: b, value))
  // CHECK-DUMP:   }
  // CHECK-DUMP: }
}
