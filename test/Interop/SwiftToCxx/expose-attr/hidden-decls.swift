// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name SwiftPrivate -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/swiftprivate.h
// RUN: %FileCheck %s < %t/swiftprivate.h

// RUN: %check-interop-cxx-header-in-clang(%t/swiftprivate.h  -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public struct Exposed {
    public var x: Int
    @_expose(!Cxx)
    public var notExposedField: Int
}

@_expose(!Cxx)
public struct NotExposed {
   public var x: Int  
}

extension NotExposed {
    func notExposed() {}
}

@_expose(!Cxx)
public func NotExposedfunction() {}

@MainActor
@_expose(!Cxx)
public class UnavailableClass {
}

// CHECK-NOT: NotExposed
// CHECK-NOT: notExposed
// CHECK: Exposed
// CHECK-NOT: UnavailableClass
