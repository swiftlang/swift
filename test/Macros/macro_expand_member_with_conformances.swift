// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-typecheck-verify-swift -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -I %t
protocol DefaultInit {
  init()
}

@attached(extension, conformances: DefaultInit)
@attached(member, conformances: DefaultInit, names: named(init()), named(f()))
macro DefaultInit() = #externalMacro(module: "MacroDefinition", type: "RequiredDefaultInitMacro")

@DefaultInit
class C { }

@DefaultInit
class D: C { }

@DefaultInit
struct E { }
