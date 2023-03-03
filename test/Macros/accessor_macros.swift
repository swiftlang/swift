// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// First check for no errors.
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir

// Check that the expansion buffer are as expected.
// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DUMP %s < %t/expansions-dump.txt

// Execution testing
// RUN: %target-build-swift -swift-version 5  -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@attached(accessor)
macro myPropertyWrapper() =
    #externalMacro(module: "MacroDefinition", type: "PropertyWrapperMacro")

struct Date { }

struct MyWrapperThingy<T> {
  var storage: T

  var wrappedValue: T {
    get {
      print("Getting value \(storage)")
      return storage
    }

    set {
      print("Setting value \(newValue)")
      storage = newValue
    }
  }
}

struct MyStruct {
  var _name: MyWrapperThingy<String> = .init(storage: "Hello")
  var _birthDate: MyWrapperThingy<Date?> = .init(storage: nil)

  @myPropertyWrapper
  var name: String
  // CHECK-DUMP: @__swiftmacro_15accessor_macros8MyStructV4nameSSvp17myPropertyWrapperfMa_.swift
  // CHECK-DUMP: get {
  // CHECK-DUMP:   _name.wrappedValue
  // CHECK-DUMP: }
  // CHECK-DUMP: set {
  // CHECK-DUMP:   _name.wrappedValue = newValue
  // CHECK-DUMP: }

  @myPropertyWrapper
  var birthDate: Date?
  // CHECK-DUMP: @__swiftmacro_15accessor_macros8MyStructV9birthDateAA0F0VSgvp17myPropertyWrapperfMa_.swift 
  // CHECK-DUMP: get {
  // CHECK-DUMP:   _birthDate.wrappedValue
  // CHECK-DUMP: }
  // CHECK-DUMP: set {
  // CHECK-DUMP:   _birthDate.wrappedValue = newValue
  // CHECK-DUMP: }
}

// Test that the fake-property-wrapper-introduced accessors execute properly at
// runtime.
var ms = MyStruct()

// CHECK: Getting value Hello
_ = ms.name

// CHECK-NEXT: Setting value World
ms.name = "World"

