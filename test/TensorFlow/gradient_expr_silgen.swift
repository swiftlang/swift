// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s | %FileCheck %s

@_silgen_name("foo")
public func foo(_ x: Float) -> Float {
  return x * x
}

public func bar() {
  let _ = #gradient(of: foo)
}

public func bar2() {
  let _ = #gradient(of: foo)
}

public func bar3() {
  let _ = #gradient(of: foo, withRespectTo: .0)
}

// CHECK-LABEL: sil @foo__grad_all : $@convention(thin) (Float) -> Float
// CHECK-NEXT: bb0
// CHECK-NEXT: autodiff_reverse @foo : $@convention(thin) (Float) -> Float

// CHECK-LABEL: sil @foo__grad_0 : $@convention(thin) (Float) -> Float
// CHECK-NEXT: bb0
// CHECK-NEXT: autodiff_reverse [wrt 0] @foo : $@convention(thin) (Float) -> Float
