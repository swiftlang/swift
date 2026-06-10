// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -disable-availability-checking -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5

// REQUIRES: swift_swift_parser

@attached(accessor)
macro BorrowMutate() = #externalMacro(module: "MacroDefinition", type: "BorrowMutateMacro")

struct MacroBased {
    private var _x: [Int] = []
    @BorrowMutate var x: [Int] 
}

func testUsage() {
    var instance = MacroBased()
    _ = instance.x
    instance.x.append(42)
}

