// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/CrossImport/A.swift -I %t -module-name A -emit-module -emit-module-path %t/
// RUN: %target-build-swift %S/Inputs/CrossImport/B.swift -I %t -module-name B -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name _A_B -I %t -emit-module -emit-module-path %t/
// RUN: cp -r %S/Inputs/CrossImport/A.swiftcrossimport %t/
// RUN: %target-swift-symbolgraph-extract -module-name A -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/A@B@_A_B.symbols.json

@_exported import A
import B

extension A {
    public func transmogrify() -> B {
        return B(y: self.x);
    }
}

// CHECK: module
// CHECK-NEXT: "name": "A"
// CHECK-NEXT: bystanders
// CHECK-NEXT:   B
