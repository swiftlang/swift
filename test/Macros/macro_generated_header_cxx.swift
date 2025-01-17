// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/objc_cxx_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-codesign %t/main

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk) -typecheck -swift-version 5 -parse-as-library -clang-header-expose-decls=all-public -emit-clang-header-path %t/MacroUser.h -load-plugin-library %t/%target-library-name(MacroDefinition) %s -module-name MacroUser
// RUN: %FileCheck --check-prefix CHECK %s < %t/MacroUser.h

@attached(member, names: prefixed(member_))
public macro MemberFunc() = #externalMacro(module: "MacroDefinition", type: "MemberFuncMacro")

@attached(peer, names: prefixed(peer_))
public macro PeerFunc() = #externalMacro(module: "MacroDefinition", type: "PeerFuncMacro")

@freestanding(declaration, names: named(cxxFreestanding))
public macro CxxFreestandingFunc() = #externalMacro(module: "MacroDefinition", type: "CxxFreestandingFuncMacro")

@freestanding(declaration, names: named(MacroExpandedStruct))
public macro CxxFreestandingStruct() = #externalMacro(module: "MacroDefinition", type: "CxxFreestandingStructMacro")

// ---

// CHECK:     class SWIFT_SYMBOL("s:9MacroUser0A14ExpandedStructV") MacroExpandedStruct final {
// CHECK-DAG: SWIFT_INLINE_THUNK void member() const SWIFT_SYMBOL("s:9MacroUser0A14ExpandedStructV6memberyyF");
#CxxFreestandingStruct

// CHECK: class SWIFT_SYMBOL("s:9MacroUser10SomeStructV") SomeStruct final {
@MemberFunc
public struct SomeStruct {
  private var someProperty: Int = 0

  @PeerFunc
  public func someMethod() {}

  #CxxFreestandingFunc

  // CHECK-DAG: SWIFT_INLINE_THUNK void peer_someMethod() const SWIFT_SYMBOL("s:9MacroUser10SomeStructV15peer_someMethodyyF");
  // CHECK-DAG: SWIFT_INLINE_THUNK void someMethod() const SWIFT_SYMBOL("s:9MacroUser10SomeStructV10someMethodyyF");
  // CHECK-DAG: SWIFT_INLINE_THUNK void cxxFreestanding() const SWIFT_SYMBOL("s:9MacroUser10SomeStructV15cxxFreestandingyyF");
  // CHECK-DAG: SWIFT_INLINE_THUNK void member_SomeStruct() const SWIFT_SYMBOL("s:9MacroUser10SomeStructV07member_cD0yyF");
}
