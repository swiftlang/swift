// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/DeprecatedSymbolsModule.swiftmodule -module-name DeprecatedSymbolsModule %S/Inputs/DeprecatedSymbolsModule.swift

// RUN: %target-typecheck-verify-swift -typecheck  -I %t

@ignoreDeprecationWarnings import DeprecatedSymbolsModule

func returnFunc() -> String {
    return importedDeprecatedFuncWithReturn()
}

func voidFunc() {
    importedDeprecatedFunc()
}

struct someType {
    let b: importedDeprecatedType
}