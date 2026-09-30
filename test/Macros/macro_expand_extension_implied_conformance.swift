// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Make sure type-checking order does not affect the conformance lookup behavior.
// RUN: %target-swift-frontend -typecheck -verify -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name main %t/a.swift %t/b.swift
// RUN: %target-swift-frontend -typecheck -verify -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name main %t/b.swift %t/a.swift

//--- a.swift
@attached(extension, conformances: ImpliesHashable)
macro AddImpliesHashable() = #externalMacro(
  module: "MacroDefinition", type: "ImpliesHashableMacro"
)

protocol ImpliesHashable: Hashable {}

func requiresHashable<T: Hashable>(_: T) {}

@AddImpliesHashable
struct S {
  struct Nested {}
}

protocol P {}
protocol ConstrainedHashable: Hashable {}

@AddImpliesHashable
struct Generic<T> {
  struct Nested {}
}

extension Generic: ConstrainedHashable where T : P {}

//--- b.swift
struct R {
  // Qualified lookup here triggers expansion of implied conformances of 'S'
  // without actually expanding the extension macro.
  var nested: S.Nested?

  // Expansion of the macro is triggered before we type-check the body, but
  // the implied Hashable conformance still has the unexpanded source until
  // we re-trigger expanding implied conformances.
  static func check(_ v: S) {
    requiresHashable(v)
  }
}

// Same case as R, except we have an existing conditional implied conformance
// to Hashable for Generic. Make sure the unconstrained one added by the
// extension macro wins.
struct Q {
  var nested: Generic<Int>.Nested?
  static func check(_ v: Generic<Int>) {
    requiresHashable(v)
  }
}
