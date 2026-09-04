// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Boundaries -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/boundaries.h
// RUN: %FileCheck %s < %t/boundaries.h
// RUN: %check-interop-cxx-header-in-clang(%t/boundaries.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public func requiresComparable<Value: Comparable>(_ value: Value) {}

public func acceptsAnyHashable(_ value: any Hashable) {}

public func returnsAnyHashable() -> any Hashable {
    return 0
}

public func returnsSomeHashable() -> some Hashable {
    return 0
}

// Only direct Hashable requirements are supported. This does not expose other
// protocols, protocol existential values, or opaque result types to C++.
// CHECK: // Unavailable in C++: Swift global function 'acceptsAnyHashable
// CHECK: // Unavailable in C++: Swift global function 'requiresComparable
// CHECK: // Unavailable in C++: Swift global function 'returnsAnyHashable
// CHECK: // Unavailable in C++: Swift global function 'returnsSomeHashable
