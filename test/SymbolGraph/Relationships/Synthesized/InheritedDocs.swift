// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name InheritedDocs -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name InheritedDocs -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json

// CHECK:            "source": "s:13InheritedDocs1PPAAE8someFuncyyF::SYNTHESIZED::s:13InheritedDocs1SV"
// CHECK-NEXT:       "target": "s:13InheritedDocs1SV"
// CHECK-NEXT:       "sourceOrigin": {
// CHECK-NEXT:         "identifier": "s:13InheritedDocs1PPAAE8someFuncyyF"
// CHECK-NEXT:         "displayName": "P.someFunc()"
// CHECK-NEXT:       }

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
