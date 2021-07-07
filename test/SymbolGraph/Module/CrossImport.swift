// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/CrossImport/A.swift -I %t -module-name A -emit-module -emit-module-path %t/
// RUN: %target-build-swift %S/Inputs/CrossImport/B.swift -I %t -module-name B -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name _A_B -I %t -emit-module -emit-module-path %t/
// RUN: cp -r %S/Inputs/CrossImport/A.swiftcrossimport %t/
// RUN: %target-swift-symbolgraph-extract -module-name A -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/_A_B@A.symbols.json --check-prefix CHECK-MOD
// RUN: %FileCheck %s --input-file %t/_A_B@A.symbols.json --check-prefix CHECK-A
// RUN: %FileCheck %s --input-file %t/_A_B@B.symbols.json --check-prefix CHECK-MOD
// RUN: %FileCheck %s --input-file %t/_A_B@B.symbols.json --check-prefix CHECK-B

@_exported import A
import B

extension A {
    public func transmogrify() -> B {
        return B(y: self.x);
    }
}

public struct LocalStruct {}

extension LocalStruct {
    public func someFunc() {}
}

extension B {
    public func untransmogrify() -> A {
        return A(x: self.y)
    }
}

// CHECK-MOD: module
// CHECK-MOD-NEXT: "name": "A"
// CHECK-MOD-NEXT: bystanders
// CHECK-MOD-NEXT:   B

// CHECK-A-NOT: s:1BAAV4_A_BE14untransmogrify1AAEVyF
// CHECK-A-DAG: s:1AAAV4_A_BE12transmogrify1BAEVyF
// CHECK-A-DAG: s:4_A_B11LocalStructV
// CHECK-A-DAG: s:4_A_B11LocalStructV8someFuncyyF

// CHECK-B: s:1BAAV4_A_BE14untransmogrify1AAEVyF
