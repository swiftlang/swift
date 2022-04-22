// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1a {
  typealias X = Int
}

// CHECK-LABEL: .P1b@
// CHECK-NEXT: Requirement signature: <Self where Self : P1a, Self.[P1b]X == Int>
protocol P1b : P1a {
  associatedtype X
}

protocol P2a {
  associatedtype A
  typealias B = Int
}

// CHECK-LABEL: .P2b@
// CHECK-NEXT: <Self where Self : P2a, Self.[P2a]A == Int>
protocol P2b : P2a where Self.A == Self.B {}

struct G<X> {}

protocol P3a {
  typealias T = G<X>
  associatedtype X
}

protocol P3b {
  typealias T = G<Y>
  associatedtype Y
}

// CHECK-LABEL: .P3c@
// CHECK-NEXT: <Self where Self : P3a, Self : P3b, Self.[P3a]X == Self.[P3b]Y>
protocol P3c : P3a, P3b {}

protocol P4a {
  typealias T = G<X>
  associatedtype X
}

// CHECK-LABEL: .P4b@
// CHECK-NEXT: <Self where Self : P4a, Self.[P4b]A == G<Self.[P4b]B>, Self.[P4b]B == Self.[P4a]X>
protocol P4b : P4a where A == T, A == G<B> {
  associatedtype A
  associatedtype B
}

// CHECK-LABEL: .P5@
// CHECK-NEXT: <Self where Self.[P5]B == Int>
protocol P5 {
  typealias A = Int
  associatedtype B where B == A
}

protocol P6a {
  typealias A = Int
  typealias B = A
}

// CHECK-LABEL: .P6b@
// CHECK-NEXT: <Self where Self : P6a, Self.[P6b]C == Int>
protocol P6b : P6a where C == B {
  associatedtype C
}

protocol P7a {
  typealias A = Int
}

protocol P7b : P7a {
  typealias B = A
}

// CHECK-LABEL: .P7c@
// CHECK-NEXT: <Self where Self : P7b, Self.[P7c]C == Int>
protocol P7c : P7b {
  associatedtype C where C == B
}

protocol P8a {
  associatedtype C
}

// CHECK-LABEL: .f1@
// CHECK-NEXT: <T, U where T : P8a, U : P7b, T.[P8a]C == Int>
func f1<T : P8a, U : P7b>(_: T, _: U) where T.C == U.B {}

// CHECK-LABEL: .f2@
// CHECK-NEXT: <T, U where T : P7b, U : P8a, U.[P8a]C == Int>
func f2<T : P7b, U : P8a>(_: T, _: U) where T.B == U.C {}
