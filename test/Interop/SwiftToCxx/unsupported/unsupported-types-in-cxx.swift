// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-decls=all-public -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

public func takeFloat(_ x: Float) {}

public func takesTuple(_ x: (Float, Float)) {}

public func takesVoid(_ x: ()) {}

// CHECK:     takeFloat

protocol TestProtocol {}

// CHECK: class TestProtocol { } SWIFT_UNAVAILABLE_MSG("protocol 'TestProtocol' can not yet be represented in C++");

// CHECK: // Unavailable in C++: Swift global function 'takesTuple(_:)'.
// CHECK: // Unavailable in C++: Swift global function 'takesVoid(_:)'.

typealias unsupportedTypeAlias = () -> (Float, Float)

// CHECK: // Unavailable in C++: Swift type alias 'unsupportedTypeAlias'
