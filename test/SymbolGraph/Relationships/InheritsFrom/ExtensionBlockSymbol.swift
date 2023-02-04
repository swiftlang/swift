// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/Base.swift -module-name Base -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name ExtensionBlockSymbol -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name ExtensionBlockSymbol -I %t -pretty-print -output-dir %t  -emit-extension-block-symbols
// RUN: %target-swift-symbolgraph-extract -module-name Base -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/Base.symbols.json --check-prefix BASE
// RUN: %FileCheck %s --input-file %t/ExtensionBlockSymbol.symbols.json --check-prefix LOCAL
// RUN: %FileCheck %s --input-file %t/ExtensionBlockSymbol@Base.symbols.json --check-prefix EXTENSION

import Base

public extension Derived {
    func foo() {}
}

// BASE: "kind": "inheritsFrom"
// BASE-NEXT: "source": "s:4Base7DerivedC"
// BASE-NEXT: "target": "s:4BaseAAC"

// LOCAL-DAG: "symbols": []
// LOCAL-DAG: "relationships": []

// EXTENSION-NOT: "kind": "inheritsFrom"
