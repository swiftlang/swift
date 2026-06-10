// RUN: %empty-directory(%t/lib)

// Build the ShadowyHorror module (contains 'public class module_shadowing').
// RUN: %target-swift-emit-module-interface(%t/lib/ShadowyHorror.swiftinterface) %S/Inputs/ShadowyHorror.swift -module-name ShadowyHorror
// RUN: %target-swift-typecheck-module-from-interface(%t/lib/ShadowyHorror.swiftinterface) -module-name ShadowyHorror

// Build this file as a module interface. Verify that no warnings are emitted
// when a typename shadows an imported module name.
// RUN: %target-swift-emit-module-interface(%t/module_shadowing.swiftinterface) %s -module-name module_shadowing -I %t/lib -verify

// Verify the generated .swiftinterface typechecks successfully.
// RUN: %target-swift-typecheck-module-from-interface(%t/module_shadowing.swiftinterface) -module-name module_shadowing -I %t/lib

import ShadowyHorror

// 'ShadowyHorror' shadows the imported module of the same name.
public struct ShadowyHorror {
  public init() {}
  public var property: ShadowyHorror { ShadowyHorror() }
}
