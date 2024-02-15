// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s

public typealias Stringified<T> = (T, String)

@freestanding(expression)
public macro publicStringify<T>(_ value: T) -> Stringified<T> = #externalMacro(
    module: "MacroDefinition", type: "StringifyMacro"
)

public func publicUsePublic(okay: Stringified<Int> = #publicStringify(0)) {}
func internalUsePublic(okay: Stringified<Int> = #publicStringify(0)) {}

// expected-note@+2{{macro 'stringify' is not '@usableFromInline' or public}}
@freestanding(expression)
macro stringify<T>(_ value: T) -> Stringified<T> = #externalMacro(
    module: "MacroDefinition", type: "StringifyMacro"
)

// expected-error@+1{{macro 'stringify' is internal and cannot be referenced from a default argument value}}
public func publicUseInternal(fail: Stringified<Int> = #stringify(0)) {}
func internalUseInternal(okay: Stringified<Int> = #stringify(0)) {}
