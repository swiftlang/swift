// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Onone -module-name main %s -o %t/main.Onone
// RUN: %target-codesign %t/main.Onone
// RUN: %target-run %t/main.Onone | %FileCheck %s
// RUN: %target-build-swift -O -module-name main %s -o %t/main.O
// RUN: %target-codesign %t/main.O
// RUN: %target-run %t/main.O | %FileCheck %s

// REQUIRES: executable_test

// https://github.com/swiftlang/swift/issues/94296
// `any Error` self-conforms to `Error`, so `(any Error).Protocol` cast to
// `any Error.Type` succeeds at runtime.  The cast optimizer used to fold this
// cast to `false` after generic specialization.

struct E: Error {}
protocol P {}
struct S: P {}

@inline(never)
func conformsToError<T>(_: T.Type) -> Bool { T.self is any Error.Type }

@inline(never)
func conformsToP<T>(_: T.Type) -> Bool { T.self is any P.Type }

@inline(never)
func asErrorType<T>(_: T.Type) -> (any Error.Type)? { T.self as? any Error.Type }

// CHECK: any Error: true
print("any Error: \(conformsToError((any Error).self))")
// CHECK-NEXT: E: true
print("E: \(conformsToError(E.self))")
// CHECK-NEXT: S: false
print("S: \(conformsToError(S.self))")
// CHECK-NEXT: as? any Error: true
print("as? any Error: \(asErrorType((any Error).self) != nil)")

// Non-self-conforming protocols must still fold to false.
// CHECK-NEXT: any P is P: false
print("any P is P: \(conformsToP((any P).self))")
// CHECK-NEXT: any Error is P: false
print("any Error is P: \(conformsToP((any Error).self))")
// CHECK-NEXT: S is P: true
print("S is P: \(conformsToP(S.self))")
