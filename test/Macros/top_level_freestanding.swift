// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -parse-as-library -enable-experimental-feature FreestandingMacros -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUNx: %target-swift-frontend -enable-experimental-feature FreestandingMacros -parse-as-library -emit-sil -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -module-name MacroUser 2>&1 | %FileCheck --check-prefix CHECK-SIL %s
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-experimental-feature FreestandingMacros -parse-as-library -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5
// RUN: %target-build-swift -g -swift-version 5 -enable-experimental-feature FreestandingMacros -parse-as-library -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

// Test unqualified lookup from within a macro expansion
@freestanding(declaration, names: named(StructWithUnqualifiedLookup))
macro structWithUnqualifiedLookup() = #externalMacro(module: "MacroDefinition", type: "DefineStructWithUnqualifiedLookupMacro")

let world = 3 // to be used by the macro expansion below

#structWithUnqualifiedLookup

func lookupGlobalFreestandingExpansion() {
  // CHECK: 4
  print(StructWithUnqualifiedLookup().foo())
}

@freestanding(declaration)
macro anonymousTypes(_: () -> String) = #externalMacro(module: "MacroDefinition", type: "DefineAnonymousTypesMacro")

#anonymousTypes { "hello" }

// CHECK-SIL: sil hidden @$s9MacroUser03$s9A35User14anonymousTypesfMf0_4namefMu0_O5helloyyF : $@convention(method) ($s9MacroUser14anonymousTypesfMf0_4namefMu0_) -> () {

@main
struct Main {
  static func main() {
    lookupGlobalFreestandingExpansion()
  }
}
