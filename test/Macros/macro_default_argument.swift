// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s

@freestanding(expression)
macro MagicLine() -> Int = #externalMacro(module: "MacroDefinition", type: "MagicLineMacro")

struct LineContainer {
    let line: Int
}

func partOfDefaultArgumentOkay(container: LineContainer = .init(line: #MagicLine)) {
    print(container.line)
}

func parenthesizedExpansionAtDeclOkay(line: Int = (#MagicLine)) {
    print(line)
}

func builtInOkay(line: Int = #line) {
    print(line)
}

// expected-error@+1{{non-built-in macro cannot be used as default argument}}
func asDefaultArgument(line: Int = #MagicLine) {
    print(line)
}

asDefaultArgument()
