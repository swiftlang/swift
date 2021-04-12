// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name InheritedDocs -emit-module -emit-module-path %t/InheritedDocs.swiftmodule -emit-module-source-info-path %t/InheritedDocs.swiftsourceinfo -emit-module-doc-path %t/InheritedDocs.swiftdoc

// RUN: %target-swift-symbolgraph-extract -module-name InheritedDocs -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes CHECK,DOCS
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes IMPL

// RUN: %target-swift-symbolgraph-extract -module-name InheritedDocs -I %t -pretty-print -output-dir %t -skip-inherited-docs
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes CHECK,SKIP
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes IMPL

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name InheritedDocs -emit-module -emit-module-path %t/InheritedDocs.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -skip-inherited-docs
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes SKIP

// DOCS-COUNT-3: Some Function
// SKIP-COUNT-1: Some Function

// CHECK:            "source": "s:13InheritedDocs1PPAAE8someFuncyyF::SYNTHESIZED::s:13InheritedDocs1SV"
// CHECK-NEXT:       "target": "s:13InheritedDocs1SV"
// CHECK-NEXT:       "sourceOrigin"
// CHECK-NEXT:         "identifier": "s:13InheritedDocs1PP8someFuncyyF"
// CHECK-NEXT:         "displayName": "P.someFunc()"

// IMPL:             "source": "s:13InheritedDocs1PPAAE8someFuncyyF"
// IMPL-NEXT:        "target": "s:13InheritedDocs1PP8someFuncyyF"
// IMPL-NEXT:        "sourceOrigin"
// IMPL-NEXT:          "identifier": "s:13InheritedDocs1PP8someFuncyyF"
// IMPL-NEXT:          "displayName": "P.someFunc()"

/// Protocol P
public protocol P {
    /// Some Function
    func someFunc()
}

public extension P {
    func someFunc() {}
}

public struct S: P {
}
