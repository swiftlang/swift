// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/ExternalTypealias.swift -module-name ExternalTypealias -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name Typealias -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name Typealias -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Typealias.symbols.json 

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/ExternalTypealias.swift -module-name ExternalTypealias -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name Typealias -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name Typealias -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Typealias@ExternalTypealias.symbols.json --check-prefix EXTERNAL

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/ExternalTypealias.swift -module-name ExternalTypealias -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name Typealias -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name Typealias -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/Typealias@ExternalTypealias.symbols.json --check-prefix EBS_EXTERNAL

public struct S {}

public typealias A = S

// Members of a typealias should be associated with the original type,
// not the typealias symbol
public extension A {
  func foo() {}
}

// CHECK: "kind": "memberOf"
// CHECK-NEXT: "source": "s:9Typealias1SV3fooyyF"
// CHECK-NEXT: "target": "s:9Typealias1SV"


// This also applies to extensions to externally defined typealiases

import ExternalTypealias

public extension ExternalA {
  func foo() {}
}

// EXTERNAL: "kind": "memberOf"
// EXTERNAL-NEXT: "source": "s:17ExternalTypealias0A1SV0B0E3fooyyF"
// EXTERNAL-NEXT: "target": "s:17ExternalTypealias0A1SV"

// EBS_EXTERNAL: "kind": "extensionTo"
// EBS_EXTERNAL-NEXT: "source": "s:e:s:17ExternalTypealias0A1SV0B0E3fooyyF"
// EBS_EXTERNAL-NEXT: "target": "s:17ExternalTypealias0A1SV"
