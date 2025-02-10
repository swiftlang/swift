// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build libraries
// RUN: %target-swift-frontend -emit-module %t/LibShared.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/LibShared.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/LibWithPublicFoo.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/LibWithPublicFoo.swiftmodule -I %t
// RUN: %target-swift-frontend -emit-module %t/LibWithInternalFoo.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/LibWithInternalFoo.swiftmodule -I %t

/// Build client
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5

//--- LibShared.swift
public struct Struct {
    public init() {}
}

//--- LibWithPublicFoo.swift
import LibShared

extension Struct {
    public func foo() {}
}

//--- LibWithInternalFoo.swift
import LibShared

extension Struct {
    internal func foo() {}
}

//--- Client.swift
import LibShared
import LibWithPublicFoo
private import LibWithInternalFoo

var s = Struct()
s.foo() // This is non-ambiguous
