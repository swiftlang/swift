// RUN: %empty-directory(%t)
// RUN: %target-build-swift -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUNx: %target-swift-frontend -dump-ast -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -module-name MacroUser 2>&1 | %FileCheck --check-prefix CHECK-AST %s
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5
// RUN: %target-build-swift -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@attached(member) macro addMembers() = #externalMacro(module: "MacroDefinition", type: "AddMembers")

@addMembers
struct S {
  func useSynthesized() {
    S.method()
    print(type(of: getStorage()))
  }
}

let s = S()

// CHECK: synthesized method
// CHECK: Storage
s.useSynthesized()

@attached(
  member,
  names: named(RawValue), named(rawValue), named(`init`)
)
public macro NewType<T>() = #externalMacro(module: "MacroDefinition", type: "NewTypeMacro")

@NewType<String>
public struct MyString {}

// CHECK: String
// CHECK: hello
let myString = MyString("hello")
print(MyString.RawValue.self)
print(myString.rawValue)
