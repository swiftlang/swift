// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/DeprecatedSymbolsModule.swiftmodule -module-name DeprecatedSymbolsModule %S/Inputs/DeprecatedSymbolsModule.swift -verify

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

    var ownX: String {
        return b.x
    }

    var t: Int {
        importedDeprecatedType.m
    }
}

func doSomething(_ b: importedDeprecatedType) { 
    _ = b.memberFunc()
    _ = importedDeprecatedVar
}

extension someType: importedDeprecatedProtocol {
    var val: String {
        return ""
    }
}

func testDeprecatedConformance<T: ImprotedNonDeprecatedProtocol>(_ x: T.Type) {}
testDeprecatedConformance(importedDeprecatedType.self)