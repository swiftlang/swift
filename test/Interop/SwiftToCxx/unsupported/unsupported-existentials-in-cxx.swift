// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Existentials -verify -clang-header-expose-decls=all-public -disable-availability-checking -typecheck -verify -emit-clang-header-path %t/existentials.h
// RUN: %FileCheck %s < %t/existentials.h

// RUN: %check-interop-cxx-header-in-clang(%t/existentials.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -std=c++17)

public struct Foo {
    var x: CInt
    var y: CInt
};

public func useExistential(_ x: KeyPath<Foo, CInt> & Sendable) {
}

// CHECK: Unavailable in C++: Swift global function 'useExistential(_:)'.
