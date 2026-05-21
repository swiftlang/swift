// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: not %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser %s 2>&1 | %FileCheck %s

@attached(member, names: arbitrary)
macro LexicalContextOuter() = #externalMacro(module: "MacroDefinition", type: "LexicalContextOuterMacro")

@attached(peer, names: arbitrary)
macro LexicalContextInner() = #externalMacro(module: "MacroDefinition", type: "LexicalContextInnerMacro")

@LexicalContextOuter
struct StructA {
  @LexicalContextOuter
  struct StructB {}

  @LexicalContextOuter
  class ClassA {}

  @LexicalContextOuter
  enum EnumA {}
}

// CHECK: lexicalContext: struct StructA
// CHECK: lexicalContext: struct StructB -> struct StructA
// CHECK: lexicalContext: class ClassA -> struct StructA
// CHECK: lexicalContext: enum EnumA -> struct StructA

extension StructA {
  @LexicalContextOuter
  struct StructC {}

  func funcA() {
    @LexicalContextInner
    func funcB() {}
  }
}

// CHECK: lexicalContext: struct StructC -> extension StructA
// CHECK: lexicalContext: func funcA -> extension StructA

@LexicalContextInner
func check() {}
