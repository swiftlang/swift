// RUN: %empty-directory(%t)
// RUN: %target-build-swift -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Check that the expansion buffer are as expected.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature Macros -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DUMP %s < %t/expansions-dump.txt

// Execution testing
// RUN: %target-build-swift -enable-experimental-feature Macros -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser
// RUN: %target-run %t/main
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@declaration(attached)
macro myPropertyWrapper: Void =
    #externalMacro(module: "MacroDefinition", type: "PropertyWrapperMacro")

struct Date { }

struct MyStruct {
  var storage: [AnyHashable: Any] = [:]
  
  @myPropertyWrapper
  var name: String
  // CHECK-DUMP: macro:name@myPropertyWrapper
  // CHECK-DUMP: get {
  // CHECK-DUMP:   _name.wrappedValue
  // CHECK-DUMP: }
  // CHECK-DUMP: set {
  // CHECK-DUMP:   _name.wrappedValue = newValue
  // CHECK-DUMP: }

  @myPropertyWrapper
  var birthDate: Date?
  // CHECK-DUMP: macro:birthDate@myPropertyWrapper
  // CHECK-DUMP: get {
  // CHECK-DUMP:   _birthDate.wrappedValue
  // CHECK-DUMP: }
  // CHECK-DUMP: set {
  // CHECK-DUMP:   _birthDate.wrappedValue = newValue
  // CHECK-DUMP: }
}

// FIXME: Actually test that the accessors got into the AST.

