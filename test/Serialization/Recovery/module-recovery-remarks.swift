// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t

/// Compile two library modules A and A_related, and a middle library LibWithXRef with a reference to a type in A.
// RUN: %target-swift-frontend %t/LibOriginal.swift -emit-module-path %t/A.swiftmodule -module-name A -I %t
// RUN: %target-swift-frontend %t/Empty.swift -emit-module-path %t/A_related.swiftmodule -module-name A_related
// RUN: %target-swift-frontend %t/LibWithXRef.swift -emit-module-path %t/sdk/LibWithXRef.swiftmodule -module-name LibWithXRef -I %t -swift-version 5 -enable-library-evolution

/// Move MyType from A to A_related, triggering most notes.
// RUN: %target-swift-frontend %t/EmptyOverlay.swift -emit-module-path %t/A.swiftmodule -module-name A -I %t
// RUN: %target-swift-frontend %t/LibOriginal.swift -emit-module-path %t/A_related.swiftmodule -module-name A_related -I %t
// RUN: not %target-swift-frontend -c -O %t/Client.swift -I %t -I %t/sdk -Rmodule-recovery -sdk %t/sdk -swift-version 4 2>&1 \
// RUN:   | %FileCheck --check-prefixes CHECK-MOVED %s

/// Main error downgraded to a remark.
// CHECK-MOVED: LibWithXRef.swiftmodule:1:1: remark: reference to type 'MyType' broken by a context change; 'MyType' was expected to be in 'A', but now a candidate is found only in 'A_related'
//--- module.modulemap
module A {
    header "A.h"
}

//--- A.h
void foo() {}

//--- Empty.swift

//--- EmptyOverlay.swift
@_exported import A

//--- LibOriginal.swift
@_exported import A

public struct MyType {
    public init() {}
}

//--- LibWithXRef.swift
import A
import A_related

public func foo() -> MyType {
    fatalError()
}

//--- Client.swift
import LibWithXRef

foo()
