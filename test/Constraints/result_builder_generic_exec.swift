// No warnings.
// RUN: %target-typecheck-verify-swift
//
// RUN: %target-run-simple-swift | %FileCheck %s
//
// REQUIRES: executable_test

@resultBuilder
struct Builder<T> {
  static func buildBlock(_ args: T...) -> [T] { args }
}

// https://github.com/swiftlang/swift/issues/72739
do {
  @Builder<T>
  func buildArray<T>(_ t1: T, _ t2: T, _ t3: T) -> [T] {
    t1
    t2
    t3
  }

  enum TypeContext {
    @Builder<T>
    static func buildArray<T>(_ t1: T, _ t2: T, _ t3: T) -> [T] {
      t1
      t2
      t3
    }
  }

  // CHECK: begin
  print("begin")
  // CHECK-NEXT: [1, 2, 3]
  print(buildArray(1, 2, 3))
  // CHECK-NEXT: [1, 2, 3]
  print(TypeContext.buildArray(1, 2, 3))
  // CHECK-NEXT: end
  print("end")
}

