// RUN: %target-typecheck-verify-swift -debug-constraints -target %target-swift-5.1-abi-triple 2>%t.err
// RUN: %FileCheck %s < %t.err

protocol P<Output> {
  associatedtype Output
}

struct S<Output> : P {
  init(_: Output) {}
}

@resultBuilder
struct Builder {
  static func buildExpression<T>(_ e: T) -> T { e }

  static func buildBlock<T1>(_ t1: T1) -> some P<T1> {
    return S(t1)
  }

  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> some P<(T1, T2)> {
    return S((t1, t2))
  }

  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3) -> some P<(T1, T2, T3)> {
    return S((t1, t2, t3))
  }

  static func buildOptional<T>(_ value: T?) -> T? { return value }
}

do {
  func test<T, U>(_: T, @Builder _: () -> some P<U>) {}

  // CHECK: ---Initial constraints for the given expression---
  // CHECK: (integer_literal_expr type="[[LITERAL_VAR:\$T[0-9]+]]" {{.*}}
  // CHECK: (attempting type variable binding [[CLOSURE:\$T[0-9]+]] := () -> {{.*}}
  // CHECK-NOT: (attempting type variable binding [[LITERAL_VAR]] := {{.*}}
  // CHECK: (attempting conjunction element pattern binding element @ 0
  // CHECK: (applying conjunction result to outer context
  // CHECK: (attempting type variable binding [[LITERAL_VAR]] := Int
  test(42) {
    1
    ""
  }
}

do {
  func test<T, U>(_: T, @Builder _: (T) -> some P<U>) {}

  // CHECK: ---Initial constraints for the given expression---
  // CHECK: (integer_literal_expr type="[[LITERAL_VAR:\$T[0-9]+]]" {{.*}}
  // CHECK: (attempting type variable binding [[LITERAL_VAR]] := Int
  // CHECK: (attempting conjunction element pattern binding element @ 0
  test(42) { v in
    v
    ""
  }
}

do {
  func test<T: BinaryInteger, U>(@Builder _: (Bool) -> some P<T>, transform: (T?) -> U) {}

  // CHECK: ---Initial constraints for the given expression---
  // CHECK: (attempting type variable {{.*}} := () -> {{.*}}
  // CHECK: (attempting conjunction element pattern binding element @ 0 :
  // CHECK-NEXT: (pattern_named "x")
  // CHECK: (attempting conjunction element syntactic element
  // CHECK-NEXT: (call_expr {{.*}}
  // CHECK: (attempting type variable {{.*}} := (Bool) -> {{.*}}
  // CHECK: (attempting conjunction element pattern binding element @ 0
  // CHECK: (pattern_named implicit "$__builder{{.*}}")
  // CHECK: (applying conjunction result to outer context
  // CHECK: (attempting type variable {{.*}} := (Int?) -> {{.*}}
  // CHECK: (attempting disjunction choice {{.*}} bound to decl {{.*}}.Int.init(_:)
  let _ = {
    let x = 42
    test { cond in
      x
    } transform: { v in
      Int(v ?? 0)
    }
  }
}
