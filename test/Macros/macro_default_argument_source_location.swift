// REQUIRES: swift_swift_parser
// REQUIRES: executable_test

// RUN: %empty-directory(%t)

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-build-swift-dylib(%t/%target-library-name(WithMacroDefaultArg)) %S/Inputs/with_macro_default_arg_module.swift -module-name WithMacroDefaultArg -emit-module -emit-module-path %t/WithMacroDefaultArg.swiftmodule -load-plugin-library %t/%target-library-name(MacroDefinition) -I %t -swift-version 5

// RUN: %target-build-swift-dylib(%t/%target-library-name(WithMacroDefaultArgInterface)) %S/Inputs/with_macro_default_arg_interface.swift -module-name WithMacroDefaultArgInterface -enable-library-evolution -emit-module-interface-path %t/WithMacroDefaultArgInterface.swiftinterface -load-plugin-library %t/%target-library-name(MacroDefinition) -I %t -swift-version 5

// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %S/Inputs/with_macro_default_arg_same_module.swift %s -o %t/main -module-name MacroUser -emit-tbd -emit-tbd-path %t/MacroUser.tbd -I %t -L %t %target-rpath(%t) -lWithMacroDefaultArg -lWithMacroDefaultArgInterface
// RUN: %target-codesign %t/main
// RUN: %target-codesign %t/%target-library-name(WithMacroDefaultArg)
// RUN: %target-codesign %t/%target-library-name(WithMacroDefaultArgInterface)
// RUN: %target-run %t/main %t/%target-library-name(WithMacroDefaultArg) %t/%target-library-name(WithMacroDefaultArgInterface) | %FileCheck %s

import WithMacroDefaultArg
import WithMacroDefaultArgInterface

struct SourceLocation: CustomStringConvertible {
    let line: Int
    let column: Int

    var description: String {
        "\(line):\(column)"
    }
}

func partOfDefaultArgumentOkay(
    // CHECK: [[# @LINE + 1]]:59
    location: SourceLocation = .init(line: #Line, column: #Column)
) {
    print(location)
}

// CHECK: [[# @LINE + 1]]
func parenthesizedExpansionAtDeclOkay(line: Int = (#Line)) {
    print(line)
}

func asDefaultArgument(line: Int = #Line) {
    print(line)
}

func asDefaultArgumentExpandingToBuiltInLine(line: Int = #MagicLine) {
    print(line)
}

@resultBuilder
enum SourceLocationBuilder {
    static func buildExpression<T>(
        _ expression: T, line: Int = #Line, column: Int = #Column
    ) -> SourceLocation {
        SourceLocation(line: line, column: column)
    }

    static func buildBlock(_ location: SourceLocation) -> SourceLocation {
        location
    }
}

func build(@SourceLocationBuilder body: () -> SourceLocation) -> SourceLocation {
    body()
}

let result = build {
    // CHECK: [[# @LINE + 1]]:5
    0
}

func sameFileID(builtIn: String = #fileID, expressionMacro: String = #FileID) {
    print(builtIn == expressionMacro)
}

func sameFilePath(builtIn: String = #filePath, expressionMacro: String = #FilePath) {
    print(builtIn == expressionMacro)
}

func sameLine(builtIn: Int = #line, expressionMacro: Int = #Line) {
    print(builtIn == expressionMacro)
}

func sameColumn(builtIn: Int = #column, expressionMacro: Int = #Column) {
    print(builtIn == expressionMacro)
}

func buildPrinter(
    @ClosureCallerBuilder makeCaller: () -> ClosureCaller
) -> ClosureCaller {
    makeCaller()
}

// CHECK: macro@MacroUser/macro_default_argument_source_location.swift#[[# @LINE + 1]]
let printWithFileLine = buildPrinter { }

@main struct Main {
    static func main() {
        partOfDefaultArgumentOkay()
        parenthesizedExpansionAtDeclOkay()
        print(result)
        printWithFileLine(context: "macro")
      
        // CHECK: hello world
        preferVariableFromLocalScope()
      
        do {
            let shadowed = 42
            // CHECK: hello 42
            preferVariableFromLocalScope()
        }
        
        // CHECK: [[# @LINE + 1]]
        asDefaultArgument()
        // CHECK: [[# @LINE + 1]]
        asDefaultArgumentExpandingToBuiltInLine()
        // CHECK: [[# @LINE + 1]]
        printCurrentLineDefinedAtAnotherFile()
        // CHECK: [[# @LINE + 1]]
        printCurrentLineDefinedAtAnotherModule()
        // CHECK: MacroUser/with_macro_default_arg_same_module.swift
        printAnotherFileName()
        // CHECK: MacroUser/macro_default_argument_source_location.swift
        printCurrentFileDefinedAtAnotherFile()
        // CHECK: MacroUser/macro_default_argument_source_location.swift
        printCurrentFileDefinedInAnotherModuleInterface()
        // CHECK: true
        sameFileID()
        // CHECK: true
        sameFilePath()
        // CHECK: true
        sameLine()
        // CHECK: true
        sameColumn()
    }
}
