// RUN: %target-swift-frontend -emit-silgen -verify %s > /dev/null
// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

struct S {
  func foo() -> Int {
    // Make sure the decl context for the autoclosure passed to ?? is deep
    // enough that it can 'see' the capture of $0 from the outer closure.
    // CHECK-LABEL: (original_init=lazy_initializer_expr
    // CHECK: (closure_expr
    // CHECK: location={{.*}}local_lazy.swift:[[@LINE+3]]
    // CHECK: (autoclosure_expr implicit
    // CHECK: captures=($0<direct>
    lazy var nest: (Int) -> Int = { Optional<Int>.none ?? $0 }
    return nest(1)
  }
}

extension S {
  func bar() -> Int {
    // CHECK-LABEL: (original_init=lazy_initializer_expr
    // CHECK: (closure_expr
    // CHECK: location={{.*}}local_lazy.swift:[[@LINE+3]]
    // CHECK: (autoclosure_expr implicit
    // CHECK: captures=($0<direct>
    lazy var nest: (Int) -> Int = { Optional<Int>.none ?? $0 }
    return nest(1)
  }
}

