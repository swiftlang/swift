// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sib -emit-module-summary-path %t/type_refs.swiftmodule.summary -module-name type_refs -Xllvm -module-summary-embed-debug-name %s
// RUN: llvm-bcanalyzer -dump %t/type_refs.swiftmodule.summary | %FileCheck %s -check-prefix BCANALYZER

// BCANALYZER-NOT: UnknownCode

// RUN: %swift-module-summary-test --to-yaml %t/type_refs.swiftmodule.summary -o %t/type_refs.summary.yaml

// Ensure that type reference to S is recorded
// RUN: cat %t/type_refs.summary.yaml | %FileCheck %s -check-prefix SIMPLE-COERCE

// SIMPLE-COERCE:      name:            '$s9type_refs9coerceToPyAA1P_pAA1SVF'
// SIMPLE-COERCE-NEXT:   guid:            15452386893050095333
// SIMPLE-COERCE-NEXT:   live:            false
// SIMPLE-COERCE-NEXT:   preserved:       false
// SIMPLE-COERCE-NEXT:   calls:           []
// SIMPLE-COERCE-NEXT:   type_refs:
// SIMPLE-COERCE-NEXT:     - name:            9type_refs1SV
// SIMPLE-COERCE-NEXT:       guid:            12736589225588998764

protocol P {
    func foo()
}

struct S : P {
    func foo() {}
} 

func coerceToP(_ x: S) -> P { return x }
