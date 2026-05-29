// Compile the same source three times and verify the resulting C++ headers are
// byte-identical.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name DeterministicOrder -clang-header-expose-decls=all-public -disable-availability-checking -typecheck -emit-clang-header-path %t/header1.h
// RUN: %target-swift-frontend %s -module-name DeterministicOrder -clang-header-expose-decls=all-public -disable-availability-checking -typecheck -emit-clang-header-path %t/header2.h
// RUN: %target-swift-frontend %s -module-name DeterministicOrder -clang-header-expose-decls=all-public -disable-availability-checking -typecheck -emit-clang-header-path %t/header3.h
// RUN: cmp %t/header1.h %t/header2.h
// RUN: cmp %t/header2.h %t/header3.h

// Sanity check: confirm the expected stub comments are actually being
// emitted, so the cmp checks above can't pass vacuously by all
// producing empty (or stub-less) headers.
// RUN: %FileCheck %s < %t/header1.h

public struct T00 {}
public struct T01 {}
public struct T02 {}
public struct T03 {}
public struct T04 {}
public struct T05 {}
public struct T06 {}
public struct T07 {}
public struct T08 {}
public struct T09 {}

infix operator <==> : ComparisonPrecedence

// Five overloads that hit the "requires code to be emitted into client" path.
@_alwaysEmitIntoClient public func <==> (lhs: T00, rhs: T00) -> Bool { false }
@_alwaysEmitIntoClient public func <==> (lhs: T01, rhs: T01) -> Bool { false }
@_alwaysEmitIntoClient public func <==> (lhs: T02, rhs: T02) -> Bool { false }
@_alwaysEmitIntoClient public func <==> (lhs: T03, rhs: T03) -> Bool { false }
@_alwaysEmitIntoClient public func <==> (lhs: T04, rhs: T04) -> Bool { false }

// Five overloads that hit the "may throw an error" path.
public func <==> (lhs: T05, rhs: T05) throws -> Bool { false }
public func <==> (lhs: T06, rhs: T06) throws -> Bool { false }
public func <==> (lhs: T07, rhs: T07) throws -> Bool { false }
public func <==> (lhs: T08, rhs: T08) throws -> Bool { false }
public func <==> (lhs: T09, rhs: T09) throws -> Bool { false }

// The ten resulting stub comments must be emitted in the same order on every run.

// CHECK-COUNT-5: // Unavailable in C++: Swift operator function '<==>(_:_:)'. operator function '<==>' can not be exposed to C++ as it requires code to be emitted into client.
// CHECK-COUNT-5: // Unavailable in C++: Swift operator function '<==>(_:_:)'. operator function '<==>' can not yet be represented in C++ as it may throw an error.
