// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name InheritedDocs -emit-module -emit-module-path %t/InheritedDocs.swiftmodule -emit-module-source-info-path %t/InheritedDocs.swiftsourceinfo -emit-module-doc-path %t/InheritedDocs.swiftdoc

// RUN: %target-swift-symbolgraph-extract -module-name InheritedDocs -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes CHECK,DOCS
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes IMPL
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes BONUS
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes BONUS-DOCS
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes EXTRA

// RUN: %target-swift-symbolgraph-extract -module-name InheritedDocs -I %t -pretty-print -output-dir %t -skip-inherited-docs
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes CHECK,SKIP
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes IMPL
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes BONUS
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes BONUS-SKIP
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes EXTRA

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name InheritedDocs -emit-module -emit-module-path %t/InheritedDocs.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -skip-inherited-docs
// RUN: %FileCheck %s --input-file %t/InheritedDocs.symbols.json --check-prefixes SKIP

// DOCS-COUNT-3: Some Function
// BONUS-DOCS-COUNT-2: Bonus docs!
// SKIP-COUNT-1: Some Function
// BONUS-SKIP-COUNT-1: Bonus docs!

// synthesized symbols should have a sourceOrigin field that points to where its docs come from

// CHECK:            "source": "s:13InheritedDocs1PPAAE8someFuncyyF::SYNTHESIZED::s:13InheritedDocs1SV"
// CHECK-NEXT:       "target": "s:13InheritedDocs1SV"
// CHECK-NEXT:       "sourceOrigin"
// CHECK-NEXT:         "identifier": "s:13InheritedDocs1PP8someFuncyyF"
// CHECK-NEXT:         "displayName": "P.someFunc()"

// non-synthesized symbols that nonetheless inherit docs (like this extension) should have the same

// IMPL:             "source": "s:13InheritedDocs1PPAAE8someFuncyyF"
// IMPL-NEXT:        "target": "s:13InheritedDocs1PP8someFuncyyF"
// IMPL-NEXT:        "sourceOrigin"
// IMPL-NEXT:          "identifier": "s:13InheritedDocs1PP8someFuncyyF"
// IMPL-NEXT:          "displayName": "P.someFunc()"

// synthesized symbols that point directly to their docs should also have a sourceOrigin field

// BONUS:            "source": "s:13InheritedDocs1PPAAE9bonusFuncyyF::SYNTHESIZED::s:13InheritedDocs1SV"
// BONUS-NEXT:       "target": "s:13InheritedDocs1SV"
// BONUS-NEXT:       "sourceOrigin"
// BONUS-NEXT:         "identifier": "s:13InheritedDocs1PPAAE9bonusFuncyyF"
// BONUS-NEXT:         "displayName": "P.bonusFunc()"

// synthesized symbols that don't have docs to inherit still need to have the sourceOrigin field

// EXTRA:         "source": "s:13InheritedDocs1PPAAE9extraFuncyyF::SYNTHESIZED::s:13InheritedDocs1SV"
// EXTRA-NEXT:    "target": "s:13InheritedDocs1SV"
// EXTRA-NEXT:    "sourceOrigin"
// EXTRA-NEXT:        "identifier": "s:13InheritedDocs1PPAAE9extraFuncyyF"
// EXTRA-NEXT:        "displayName": "P.extraFunc()"

/// Protocol P
public protocol P {
    /// Some Function
    func someFunc()
}

public extension P {
    func someFunc() {}

    /// Bonus docs!
    func bonusFunc() {}

    func extraFunc() {} // no docs, but still needs sourceOrigin
}

public struct S: P {
}
