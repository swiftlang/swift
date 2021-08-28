// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/RemoteP.swift -module-name RemoteP -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name Remote -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name Remote -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Remote.symbols.json
// RUN: %FileCheck %s --input-file %t/Remote.symbols.json --check-prefix MEMBER

import RemoteP

public protocol LocalP: RemoteP {}

public extension LocalP {
    func someFunc() {}
}

// default implementations that are for protocols in a different module should have a `memberOf`
// relation linking them to a local symbol, if one exists

// CHECK:           "kind": "defaultImplementationOf"
// CHECK-NEXT:      "source": "s:6Remote6LocalPPAAE8someFuncyyF"
// CHECK-NEXT:      "target": "s:7RemotePAAP8someFuncyyF"
// CHECK-NEXT:      "targetFallback": "RemoteP.RemoteP.someFunc()"

// MEMBER:           "kind": "memberOf"
// MEMBER-NEXT:      "source": "s:6Remote6LocalPPAAE8someFuncyyF"
// MEMBER-NEXT:      "target": "s:6Remote6LocalPP"
