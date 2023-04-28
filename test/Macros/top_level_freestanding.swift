// REQUIRES: swift_swift_parser, executable_test
//
// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -parse-as-library -enable-experimental-feature FreestandingMacros -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -enable-experimental-feature FreestandingMacros -parse-as-library -emit-sil -load-plugin-library %t/%target-library-name(MacroDefinition) %s -module-name MacroUser 2>&1 | %FileCheck --check-prefix CHECK-SIL %s

// Type check testing
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-experimental-feature FreestandingMacros -parse-as-library -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5  %S/Inputs/top_level_freestanding_other.swift

// Check diagnostic buffer names
// RUN: %target-swift-frontend -typecheck -swift-version 5 -enable-experimental-feature FreestandingMacros -parse-as-library -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5 %s %S/Inputs/top_level_freestanding_other.swift 2> %t.diags
// RUN: %FileCheck -check-prefix DIAG_BUFFERS %s < %t.diags

// Execution testing
// RUN: %target-build-swift -g -swift-version 5 -enable-experimental-feature FreestandingMacros -parse-as-library -load-plugin-library %t/%target-library-name(MacroDefinition) %s %S/Inputs/top_level_freestanding_other.swift -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

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
macro anonymousTypes(public: Bool = false, _: () -> String) = #externalMacro(module: "MacroDefinition", type: "DefineAnonymousTypesMacro")

#anonymousTypes(public: true) { "hello" }

// CHECK-SIL: sil @$s9MacroUser03$s9A71User33_082AE7CFEFA6960C804A9FE7366EB5A0Ll14anonymousTypesfMf0_4namefMu_C5helloSSyF

@main
struct Main {
  static func main() {
    lookupGlobalFreestandingExpansion()
  }
}

@freestanding(declaration)
macro freestandingWithClosure<T>(_ value: T, body: (T) -> T) = #externalMacro(module: "MacroDefinition", type: "EmptyDeclarationMacro")

// Unqualified lookup for names defined within macro arguments.
#freestandingWithClosure(0) { x in x }

#freestandingWithClosure(1) {
  let x = $0
  return x
}

struct HasInnerClosure {
  #freestandingWithClosure(0) { x in x }
  #freestandingWithClosure(1) { x in x }
}

// Arbitrary names at global scope

@freestanding(declaration, names: arbitrary) macro bitwidthNumberedStructs(_ baseName: String) = #externalMacro(module: "MacroDefinition", type: "DefineBitwidthNumberedStructsMacro")

#bitwidthNumberedStructs("MyIntGlobal")

func testArbitraryAtGlobal() {
  _ = MyIntGlobal16()
}

@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

// DIAG_BUFFERS: @__swiftmacro_9MacroUser33_{{.*}}9stringifyfMf1_{{.*}}warning: 'deprecated()' is deprecated
// DIAG_BUFFERS: @__swiftmacro_9MacroUser33_{{.*}}9stringifyfMf2_{{.*}}warning: 'deprecated()' is deprecated
