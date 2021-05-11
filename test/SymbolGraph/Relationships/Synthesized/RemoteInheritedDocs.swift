// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/RemoteP.swift -module-name RemoteP -emit-module -emit-module-path %t/RemoteP.swiftmodule -emit-module-source-info-path %t/RemoteP.swiftsourceinfo -emit-module-doc-path %t/RemoteP.swiftdoc
// RUN: %target-swift-frontend %s -module-name RemoteInheritedDocs -emit-module -emit-module-path %t/RemoteInheritedDocs.swiftmodule -emit-module-source-info-path %t/RemoteInheritedDocs.swiftsourceinfo -emit-module-doc-path %t/RemoteInheritedDocs.swiftdoc -I %t

// RUN: %target-swift-symbolgraph-extract -module-name RemoteInheritedDocs -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix SOME
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix OTHER
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix BONUS
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix INHERIT
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix LOCAL
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix OVERRIDE

// RUN: %target-swift-symbolgraph-extract -module-name RemoteInheritedDocs -I %t -pretty-print -output-dir %t -skip-inherited-docs
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix SOME
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix OTHER
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix BONUS
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix SKIP
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix LOCAL
// RUN: %FileCheck %s --input-file %t/RemoteInheritedDocs.symbols.json --check-prefix OVERRIDE

// SOME:           "source": "s:19RemoteInheritedDocs1SV8someFuncyyF"
// SOME-NEXT:      "target": "s:19RemoteInheritedDocs1SV"
// SOME-NEXT:      "sourceOrigin"
// SOME-NEXT:        "identifier": "s:7RemoteP1PP8someFuncyyF"
// SOME-NEXT:        "displayName": "P.someFunc()"

// OTHER:           "source": "s:19RemoteInheritedDocs1SV9otherFuncyyF"
// OTHER-NEXT:      "target": "s:19RemoteInheritedDocs1SV"
// OTHER-NEXT:      "sourceOrigin"
// OTHER-NEXT:        "identifier": "s:7RemoteP1PP9otherFuncyyF"
// OTHER-NEXT:        "displayName": "P.otherFunc()"

// BONUS:           "source": "s:19RemoteInheritedDocs1SV9bonusFuncyyF"
// BONUS-NEXT:      "target": "s:19RemoteInheritedDocs1SV"
// BONUS-NEXT:      "sourceOrigin"
// BONUS-NEXT:        "identifier": "s:7RemoteP1PP9bonusFuncyyF"
// BONUS-NEXT:        "displayName": "P.bonusFunc()"

// INHERIT: This one has docs!
// SKIP-NOT: This one has docs!

// LOCAL: Local docs override!

// OVERRIDE-NOT: Extra default docs!
// OVERRIDE-NOT: Extension override!

import RemoteP

// The `RemoteP.P` protocol has three methods: `someFunc` and `bonusFunc` don't have docs upstream,
// but `otherFunc` does. Regardless, each one needs a `sourceOrigin` field connecting it to
// upstream.

// `RemoteP.P` also has an extension with a default implementation for `extraFunc` that does have
// docs, but overriding it here should prevent those from appearing

public struct S: P {
    public func someFunc() {}

    public func otherFunc() {}

    /// Local docs override!
    public func bonusFunc() {}

    public func extraFunc() {}
}

public extension P {
    /// Extension override!
    func someFunc() {}
}

