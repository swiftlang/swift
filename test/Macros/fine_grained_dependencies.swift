// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift

// RUN: %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -parse-as-library -emit-reference-dependencies-path %t/MacroUser.swiftdeps -primary-file %s

// Note: this test ensures that we don't crash when trying to mangle a symbol
// within a closure passed to a macro.

@freestanding(declaration)
macro Empty<T>(_ x: T) = #externalMacro(module: "MacroDefinition", type: "EmptyDeclarationMacro")

#Empty {
    struct S {
        static var foo: Int { 0 }
    }
    _ = S.foo
}
