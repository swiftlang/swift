// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

protocol P {
  associatedtype X : P
}

// Anything that mentions 'T : P' minimizes to 'U : P'.

// expected-warning@+2 {{redundant conformance constraint 'U' : 'P'}}
// expected-note@+1 {{conformance constraint 'U' : 'P' implied here}}
func oneProtocol1<T, U>(_: T, _: U) where T : P, U : P, T.X == U, U.X == T {}
// CHECK-LABEL: oneProtocol1
// CHECK: Generic signature: <T, U where T : P, T == U.X, U == T.X>

// expected-warning@+2 {{redundant conformance constraint 'U' : 'P'}}
// expected-note@+1 {{conformance constraint 'U' : 'P' implied here}}
func oneProtocol2<T, U>(_: T, _: U) where U : P, T : P, T.X == U, U.X == T {}
// CHECK-LABEL: oneProtocol2
// CHECK: Generic signature: <T, U where T : P, T == U.X, U == T.X>

// expected-warning@+2 {{redundant conformance constraint 'U' : 'P'}}
// expected-note@+1 {{conformance constraint 'U' : 'P' implied here}}
func oneProtocol3<T, U>(_: T, _: U) where T : P, T.X == U, U : P, U.X == T {}
// CHECK-LABEL: oneProtocol3
// CHECK: Generic signature: <T, U where T : P, T == U.X, U == T.X>

// expected-warning@+2 {{redundant conformance constraint 'U' : 'P'}}
// expected-note@+1 {{conformance constraint 'U' : 'P' implied here}}
func oneProtocol4<T, U>(_: T, _: U) where U : P, T.X == U, T : P, U.X == T {}
// CHECK-LABEL: oneProtocol4
// CHECK: Generic signature: <T, U where T : P, T == U.X, U == T.X>

func oneProtocol5<T, U>(_: T, _: U) where T : P, T.X == U, U.X == T {}
// CHECK-LABEL: oneProtocol5
// CHECK: Generic signature: <T, U where T : P, T == U.X, U == T.X>

func oneProtocol6<T, U>(_: T, _: U) where T.X == U, U.X == T, T : P {}
// CHECK-LABEL: oneProtocol6
// CHECK: Generic signature: <T, U where T : P, T == U.X, U == T.X>

// Anything that mentions 'U : P' but not 'T : P' minimizes to 'U : P'.

// FIXME: Need to emit warning here too
func oneProtocol7<T, U>(_: T, _: U) where U : P, T.X == U, U.X == T {}
// CHECK-LABEL: oneProtocol7
// CHECK: Generic signature: <T, U where T == U.X, U : P, U == T.X>

// FIXME: Need to emit warning here too
func oneProtocol8<T, U>(_: T, _: U) where T.X == U, U.X == T, U : P {}
// CHECK-LABEL: oneProtocol8
// CHECK: Generic signature: <T, U where T == U.X, U : P, U == T.X>

protocol P1 {
  associatedtype X : P2
}

protocol P2 {
  associatedtype Y : P1
}

// expected-warning@+2 {{redundant conformance constraint 'U' : 'P2'}}
// expected-note@+1 {{conformance constraint 'U' : 'P2' implied here}}
func twoProtocols1<T, U>(_: T, _: U) where T : P1, U : P2, T.X == U, U.Y == T {}
// CHECK-LABEL: twoProtocols1
// CHECK: Generic signature: <T, U where T : P1, T == U.Y, U == T.X>

// expected-warning@+2 {{redundant conformance constraint 'U' : 'P2'}}
// expected-note@+1 {{conformance constraint 'U' : 'P2' implied here}}
func twoProtocols2<T, U>(_: T, _: U) where U : P2, T : P1, T.X == U, U.Y == T {}
// CHECK-LABEL: twoProtocols2
// CHECK: Generic signature: <T, U where T : P1, T == U.Y, U == T.X>

// expected-warning@+2 {{redundant conformance constraint 'U' : 'P2'}}
// expected-note@+1 {{conformance constraint 'U' : 'P2' implied here}}
func twoProtocols3<T, U>(_: T, _: U) where T : P1, T.X == U, U : P2, U.Y == T {}
// CHECK-LABEL: twoProtocols3
// CHECK: Generic signature: <T, U where T : P1, T == U.Y, U == T.X>

// expected-warning@+2 {{redundant conformance constraint 'U' : 'P2'}}
// expected-note@+1 {{conformance constraint 'U' : 'P2' implied here}}
func twoProtocols4<T, U>(_: T, _: U) where U : P2, T.X == U, T : P1, U.Y == T {}
// CHECK-LABEL: twoProtocols4
// CHECK: Generic signature: <T, U where T : P1, T == U.Y, U == T.X>

// expected-warning@+2 {{redundant conformance constraint 'U' : 'P2'}}
// expected-note@+1 {{conformance constraint 'U' : 'P2' implied here}}
func twoProtocols5<T, U>(_: T, _: U) where T : P1, T.X == U, U.Y == T, U : P2 {}
// CHECK-LABEL: twoProtocols5
// CHECK: Generic signature: <T, U where T : P1, T == U.Y, U == T.X>

// expected-warning@+2 {{redundant conformance constraint 'T' : 'P1'}}
// expected-note@+1 {{conformance constraint 'T' : 'P1' implied here}}
func twoProtocols6<T, U>(_: T, _: U) where U : P2, T.X == U, U.Y == T, T : P1 {}
// CHECK-LABEL: twoProtocols6
// CHECK: Generic signature: <T, U where T == U.Y, U : P2, U == T.X>
