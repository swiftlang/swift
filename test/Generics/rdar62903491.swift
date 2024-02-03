// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

protocol P {
  associatedtype X : P
}

// Anything that mentions 'T : P' and 'U : P' minimizes to 'U : P'.

func oneProtocol1<T, U>(_: T, _: U) where T : P, U : P, T.X == U, U.X == T {}
// CHECK-LABEL: oneProtocol1
// CHECK: Generic signature: <T, U where T : P, T == U.[P]X, U == T.[P]X>

func oneProtocol2<T, U>(_: T, _: U) where U : P, T : P, T.X == U, U.X == T {}
// CHECK-LABEL: oneProtocol2
// CHECK: Generic signature: <T, U where T : P, T == U.[P]X, U == T.[P]X>

func oneProtocol3<T, U>(_: T, _: U) where T : P, T.X == U, U : P, U.X == T {}
// CHECK-LABEL: oneProtocol3
// CHECK: Generic signature: <T, U where T : P, T == U.[P]X, U == T.[P]X>

func oneProtocol4<T, U>(_: T, _: U) where U : P, T.X == U, T : P, U.X == T {}
// CHECK-LABEL: oneProtocol4
// CHECK: Generic signature: <T, U where T : P, T == U.[P]X, U == T.[P]X>

// Anything that only mentions 'T : P' minimizes to 'T : P'.

func oneProtocol5<T, U>(_: T, _: U) where T : P, T.X == U, U.X == T {}
// CHECK-LABEL: oneProtocol5
// CHECK: Generic signature: <T, U where T : P, T == U.[P]X, U == T.[P]X>

func oneProtocol6<T, U>(_: T, _: U) where T.X == U, U.X == T, T : P {}
// CHECK-LABEL: oneProtocol6
// CHECK: Generic signature: <T, U where T : P, T == U.[P]X, U == T.[P]X>

// Anything that only mentions 'U : P' minimizes to 'U : P'.

func oneProtocol7<T, U>(_: T, _: U) where U : P, T.X == U, U.X == T {}
// CHECK-LABEL: oneProtocol7
// CHECK: Generic signature: <T, U where T == U.[P]X, U : P, U == T.[P]X>

func oneProtocol8<T, U>(_: T, _: U) where T.X == U, U.X == T, U : P {}
// CHECK-LABEL: oneProtocol8
// CHECK: Generic signature: <T, U where T == U.[P]X, U : P, U == T.[P]X>

protocol P1 {
  associatedtype X : P2
}

protocol P2 {
  associatedtype Y : P1
}

func twoProtocols1<T, U>(_: T, _: U) where T : P1, U : P2, T.X == U, U.Y == T {}
// CHECK-LABEL: twoProtocols1
// CHECK: Generic signature: <T, U where T : P1, T == U.[P2]Y, U == T.[P1]X>

func twoProtocols2<T, U>(_: T, _: U) where U : P2, T : P1, T.X == U, U.Y == T {}
// CHECK-LABEL: twoProtocols2
// CHECK: Generic signature: <T, U where T : P1, T == U.[P2]Y, U == T.[P1]X>

func twoProtocols3<T, U>(_: T, _: U) where T : P1, T.X == U, U : P2, U.Y == T {}
// CHECK-LABEL: twoProtocols3
// CHECK: Generic signature: <T, U where T : P1, T == U.[P2]Y, U == T.[P1]X>

func twoProtocols4<T, U>(_: T, _: U) where U : P2, T.X == U, T : P1, U.Y == T {}
// CHECK-LABEL: twoProtocols4
// CHECK: Generic signature: <T, U where T : P1, T == U.[P2]Y, U == T.[P1]X>

func twoProtocols5<T, U>(_: T, _: U) where T : P1, T.X == U, U.Y == T, U : P2 {}
// CHECK-LABEL: twoProtocols5
// CHECK: Generic signature: <T, U where T : P1, T == U.[P2]Y, U == T.[P1]X>

// The GenericSignatureBuilder minimized this signature down to
// <T, U where T == U.[P2]Y, U : P2, U == T.[P1]X>.
//
// The Requirement Machine instead emits
// <T, U where T : P1, T == U.[P2]Y, U == T.[P1]X>.
//
// This is a hypothetical ABI break, but it is such a silly edge case that
// it shouldn't matter in practice. Given that either of the two conformance
// requirements here are redundant, the user can omit one or the other to
// specify the result that they desire.

func twoProtocols6<T, U>(_: T, _: U) where U : P2, T.X == U, U.Y == T, T : P1 {}
// CHECK-LABEL: twoProtocols6
// CHECK: Generic signature: <T, U where T : P1, T == U.[P2]Y, U == T.[P1]X>
