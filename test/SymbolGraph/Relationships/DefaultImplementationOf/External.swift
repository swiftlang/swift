// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/RemoteP.swift -module-name RemoteP -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name External -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name External -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/External@RemoteP.symbols.json
// RUN: %FileCheck %s --input-file %t/External@RemoteP.symbols.json --check-prefix MEMBER

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/RemoteP.swift -module-name RemoteP -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name External -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name External -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/External@RemoteP.symbols.json
// RUN: %FileCheck %s --input-file %t/External@RemoteP.symbols.json --check-prefix MEMBEREBS
// RUN: %FileCheck %s --input-file %t/External@RemoteP.symbols.json --check-prefix EXTENSIONTOEBS

import RemoteP

public extension RemoteP {
    func someFunc() {}
}

// Default implementations that are for protocols in a different module should have a `memberOf`
// relation linking them to a local symbol. If the default implementation is not defined on a local
// protocol, the extension block symbol defining the default implementation should be the target
// of the memberOf relationship.

// CHECK:           "kind": "defaultImplementationOf"
// CHECK-NEXT:      "source": "s:7RemotePAAP8ExternalE8someFuncyyF"
// CHECK-NEXT:      "target": "s:7RemotePAAP8someFuncyyF"
// CHECK-NEXT:      "targetFallback": "RemoteP.RemoteP.someFunc()"

// MEMBER:           "kind": "memberOf"
// MEMBER-NEXT:      "source": "s:7RemotePAAP8ExternalE8someFuncyyF"
// MEMBER-NEXT:      "target": "s:7RemotePAAP"
// MEMBER-NEXT:      "targetFallback": "RemoteP.RemoteP"

// MEMBEREBS:        "kind": "memberOf"
// MEMBEREBS-NEXT:   "source": "s:7RemotePAAP8ExternalE8someFuncyyF"
// MEMBEREBS-NEXT:   "target": "s:e:s:7RemotePAAP8ExternalE8someFuncyyF"
// MEMBEREBS-NEXT:   "targetFallback": "RemoteP.RemoteP"

// EXTENSIONTOEBS:      "kind": "extensionTo"
// EXTENSIONTOEBS-NEXT: "source": "s:e:s:7RemotePAAP8ExternalE8someFuncyyF"
// EXTENSIONTOEBS-NEXT: "target": "s:7RemotePAAP"
// EXTENSIONTOEBS-NEXT: "targetFallback": "RemoteP.RemoteP"
