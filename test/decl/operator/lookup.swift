// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %S/Inputs/lookup_moduleD.swift -module-name D -o %t -I %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/lookup_moduleC.swift -module-name C -o %t -I %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/lookup_moduleB.swift -module-name B -o %t -I %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/lookup_moduleA.swift -module-name A -o %t -I %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/lookup_module_exportsAC.swift -module-name ExportsAC -o %t -I %t
// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/lookup_other.swift %S/Inputs/lookup_other2.swift %S/Inputs/lookup_other_noncompat.swift -I %t -enable-new-operator-lookup

import ExportsAC
import B

infix operator ^^^ : DeclaredAcrossFiles
func ^^^ (lhs: Int, rhs: Int) -> Int { 0 }
func &&& (lhs: Int, rhs: Int) -> Int { 0 }

// The operator decl >>> is declared in module A, which we should be able to
// see through ExportsAC.
prefix func >>> (rhs: Double) {}

// Make sure we can also see precedencegroups in module A through ExportsAC.
infix operator ^^^^ : DeclaredInModuleA

// The operator decl for ??? is declared in both modules A and B, but has the
// same default precedence group in both, so there's no ambiguity.
func ??? (lhs: Int, rhs: Int) {}

// Same for ???!, declared in modules ExportsAC and B, but has the same
// precedence group in both.
func ???! (lhs: Int, rhs: Int) {}

// The operator decl for ???? is declared in both modules A and B, and has a
// different precedence group in each. Therefore ambiguous.
func ???? (lhs: Int, rhs: Int) {} // expected-error {{ambiguous operator declarations found for operator}}

// Same for ????!, declared in both modules ExportsAC and B, and has a different
// precedence group in each. Therefore ambiguous.
func ????! (lhs: Int, rhs: Int) {} // expected-error {{ambiguous operator declarations found for operator}}

// The precedencegroup is declared in both modules A and B, therefore ambiguous.
infix operator <?> : DeclaredInModulesAB // expected-error {{multiple precedence groups found}}

// The precedencegroup is declared in both modules ExportsAC and B, therefore
// ambiguous.
infix operator <!> : DeclaredInModulesBExportsAC // expected-error {{multiple precedence groups found}}

// While this precedencegroup is declared in both modules A and B, it's also
// declared in this module, which therefore shadows those decls.
infix operator <??> : DeclaredInModulesABShadowed

// The operator decl for <? is declared in both modules A and B, but there's no
// meaningful difference between the declarations, so legal.
postfix func <? (lhs: Int) {}

// Same thing, <! is declared in both modules ExportsAC and B, but there's no
// meaningful difference between the declarations, so legal.
postfix func <! (lhs: Int) {}

// This precedencegroup is declared in both modules A and ExportsAC, but the
// latter shadows the former.
infix operator <???> : ShadowsModuleA

// This precedencegroup is declared in modules A, C, and ExportsAC, but the
// latter shadows both of the former.
infix operator <????> : ShadowsModulesAC

// This operator decl is declared in modules A, C, and ExportsAC, but the
// latter shadows both of the former.
func ????? (lhs: Int, rhs: Int) {}

// This operator decl is declared in modules A, C, and ExportsAC, but the
// latter shadows both of the former, despite them having different
// precedencegroups.
func ?????? (lhs: Int, rhs: Int) {}

// Module D is imported through exports in both lookup_other and lookup_other2.
// Make sure we correctly handle visiting the same module twice.
infix operator <> : DeclaredInModuleD

// Also declared in lookup_other.
precedencegroup RedeclaredInModule {}
// expected-error@-1 {{precedence group redeclared}}
// expected-note@-2 {{found this matching precedence group}}

infix operator *** : RedeclaredInModule // expected-error {{multiple precedence groups found}}

func testOperatorLookup() {
  // In lookup_other, DeclaredAcrossFiles is left associative, whereas in
  // module B it is non-associative. Make sure we use lookup_other's.
  _ = 5 ^^^ 5 ^^^ 5 // Okay.

  // Same for &&&, in lookup_other it is declared as left associative.
  _ = 5 &&& 5 &&& 5

  // The operator >>> is declared in module A, which we should be able to see
  // through ExportsAC.
  >>>1

  // We've been evil and overriden TernaryPrecedence in both modules A and B.
  // Make sure we emit an ambiguity error without emitting a 'broken stdlib'
  // error.
  true ? () : () // expected-error {{multiple precedence groups found}}
}

precedencegroup CastingPrecedence {
  lowerThan: AssignmentPrecedence
}

func testBuiltinPrecedenceGroupOverriding() {
  // Evil, but allowed.
  var x = 0
  x = 0 as Int // expected-error {{cannot convert value of type '()' to type 'Int' in coercion}}
}
