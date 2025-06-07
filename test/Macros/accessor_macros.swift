// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Check for expected errors.
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -DTEST_DIAGNOSTICS -verify-ignore-unknown
// RUN: not %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -DTEST_DIAGNOSTICS %s > %t/diags.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DIAGS %s < %t/diags.txt

// Check that the expansion buffer are as expected.
// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %s -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DUMP %s < %t/expansions-dump.txt

// Execution testing
// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

@attached(accessor)
macro myPropertyWrapper() =
    #externalMacro(module: "MacroDefinition", type: "PropertyWrapperMacro")

@attached(accessor)
macro myPropertyWrapperSkipsComputed() =
    #externalMacro(module: "MacroDefinition", type: "PropertyWrapperSkipsComputedMacro")

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
  var _favoriteColor: MyWrapperThingy<String> = .init(storage: "Blue")

  @myPropertyWrapper
  var name: String
  // CHECK-DUMP: @__swiftmacro_15accessor_macros8MyStructV4name17myPropertyWrapperfMa_.swift
  // CHECK-DUMP: get {
  // CHECK-DUMP:   _name.wrappedValue
  // CHECK-DUMP: }
  // CHECK-DUMP: set {
  // CHECK-DUMP:   _name.wrappedValue = newValue
  // CHECK-DUMP: }

  @myPropertyWrapper
  var birthDate: Date?
  // CHECK-DUMP: @__swiftmacro_15accessor_macros8MyStructV9birthDate17myPropertyWrapperfMa_.swift
  // CHECK-DUMP: get {
  // CHECK-DUMP:   _birthDate.wrappedValue
  // CHECK-DUMP: }
  // CHECK-DUMP: set {
  // CHECK-DUMP:   _birthDate.wrappedValue = newValue
  // CHECK-DUMP: }

  @myPropertyWrapperSkipsComputed
  var age: Int? {
    get { nil }
  }

  @myPropertyWrapper
  var favoriteColor: String {
    didSet { fatalError("Boom") }
  }
}

// Test that the fake-property-wrapper-introduced accessors execute properly at
// runtime.
var ms = MyStruct()

// CHECK: Getting value Hello
_ = ms.name

// CHECK-NEXT: Setting value World
ms.name = "World"

// CHECK-NEXT: Setting value Yellow
ms.favoriteColor = "Yellow"

#if TEST_DIAGNOSTICS
struct MyBrokenStruct {
  var _birthDate: MyWrapperThingy<Date?> = .init(storage: nil)

  // expected-note@+1 2{{in expansion of macro 'myPropertyWrapper' on property 'birthDate' here}}
  @myPropertyWrapper
  var birthDate: Date? {
    // CHECK-DIAGS: variable already has a getter
    // CHECK-DIAGS: in expansion of macro
    // CHECK-DIAGS: previous definition of getter here
    get { fatalError("Boom") }
    // expected-note @-1{{previous definition of getter here}}

    // CHECK-DIAGS: variable already has a setter
    // CHECK-DIAGS: in expansion of macro
    // CHECK-DIAGS: previous definition of setter here
    set { fatalError("Boom") }
    // expected-note @-1{{previous definition of setter here}}
  }
}

// expected-error@+1{{'accessor' macro cannot be attached to struct ('CannotHaveAccessors')}}
@myPropertyWrapper
struct CannotHaveAccessors {}
// CHECK-DIAGS: 'accessor' macro cannot be attached to struct ('CannotHaveAccessors')
#endif



@attached(accessor, names: named(willSet))
macro SkipsComputed() =
    #externalMacro(module: "MacroDefinition", type: "PropertyWrapperSkipsComputedMacro")

struct HasComputed {
  @SkipsComputed
  var value: Int { 17 }
}

@attached(accessor, names: named(willSet))
macro AddWillSet() =
    #externalMacro(module: "MacroDefinition", type: "WillSetMacro")

@attached(accessor)
macro AddWillSetSneakily() =
    #externalMacro(module: "MacroDefinition", type: "WillSetMacro")

@attached(accessor, names: named(willSet))
macro MakeComputedSneakily() =
    #externalMacro(module: "MacroDefinition", type: "PropertyWrapperMacro")

struct HasStoredTests {
  @AddWillSet var x: Int = 0

#if TEST_DIAGNOSTICS
  @AddWillSetSneakily var y: Int = 0
  // expected-error@-1{{expansion of macro 'AddWillSetSneakily()' did not produce a non-observing accessor (such as 'get') as expected}}

  @MakeComputedSneakily var z: Int = 0
  // expected-error@-1{{expansion of macro 'MakeComputedSneakily()' produced an unexpected getter}}
  // expected-note@-2 2{{in expansion of macro}}
  // expected-note@-3 2{{'z' declared here}}
#endif
}


#if TEST_DIAGNOSTICS
struct MultipleVars {
  @AddWillSet var (x, y): (Int, Int) = (0, 0)
  // expected-error@-1 2{{accessor macro 'AddWillSet()' can only apply to a single variable}}
}
#endif

@attached(accessor)
macro addGetterMacro() =
    #externalMacro(module: "MacroDefinition", type: "AddGetterMacro")

#if TEST_DIAGNOSTICS
struct S {
  @addGetterMacro let x: Int
  // expected-warning@-1 {{cannot expand accessor macro on variable declared with 'let'; this is an error in the Swift 6 language mode}}
}
#endif

func acceptAutoclosure(_ success: @autoclosure () -> Bool, message: @autoclosure () -> String) {
}

@attached(accessor)
macro BigEndianAccessorMacro() = #externalMacro(module: "MacroDefinition", type: "BigEndianAccessorMacro")

func testLocalWithAutoclosure(x: Int, y: Int) {
  struct Local {
    var __value: Int = 0

    // CHECK-DUMP: @__swiftmacro_15accessor_macros9value_$l022BigEndianAccessorMacrofMa_.swift
    @BigEndianAccessorMacro
    var value: Int
  }

  acceptAutoclosure(x == y, message: "they better be the same")

  let local = Local(__value: 5)
  acceptAutoclosure(x + 1 == local.__value, message: "they better be the same")

  if x == y {
    struct Nested {
      struct Local {
        var __value: Int = 0

        // CHECK-DUMP: @__swiftmacro_15accessor_macros9value_$l122BigEndianAccessorMacrofMa_.swift
        @BigEndianAccessorMacro
        var value: Int
      }
    }
  }
}
