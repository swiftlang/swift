// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -I %swift-lib-dir -L %swift-lib-dir -emit-library -emit-library-path=%t/%target-library-name(MacroDefinitionMissingAllMacros) -working-directory=%t -module-name=MacroDefinitionMissingAllMacros %S/Inputs/macro_definition_missing_allmacros.swift
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -I %swift-lib-dir -L %swift-lib-dir -emit-library -emit-library-path=%t/%target-library-name(MacroDefinition) -working-directory=%t -module-name=MacroDefinition %S/Inputs/macro_definition.swift
// RUN: %target-swift-frontend -I %swift-lib-dir -L %swift-lib-dir -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -load-plugin-library %t/%target-library-name(MacroDefinitionMissingAllMacros) -disable-availability-checking -typecheck -verify -primary-file %s

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

// rdar://102160067
// UNSUPPORTED: CPU=arm64e

macro customStringify<T>(_ value: T) -> (T, String) = MacroDefinition.StringifyMacro

// expected-note @+2 {{test note}}
// expected-warning @+1 {{test warning}}
let _ = #customStringify(1.byteSwapped + 2.advanced(by: 10))

// expected-note @+2 {{test note}}
// expected-warning @+1 {{test warning}}
let _ = #customStringify(1.0.truncatingRemainder(dividingBy: 1.0) + 3.0)

// expected-note @+1 {{test note}}
let _ = #customStringify(
  ["a", "b", "c"] +
// expected-warning @+1 {{test warning}}
  ["d", "e", "f"])

// expected-error @+1 {{macro 'notDefined' is undefined; use `-load-plugin-library` to specify dynamic libraries that contain this macro}}
let _ = #notDefined

// expected-error @+1 {{macro 'dummy' is undefined; use `-load-plugin-library` to specify dynamic libraries that contain this macro}}
let _ = #dummy
