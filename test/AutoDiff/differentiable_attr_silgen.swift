// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-sil -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-sil -verify %s | %FileCheck %s

@_silgen_name("foo")
@differentiable(gradient: dfoo(_:partial:seed:))
public func foo(_ x: Float) -> Float {
  return x * x
}

// CHECK-LABEL: sil [differentiable wrt 0 adjoint @dfoo] @foo

@_silgen_name("dfoo")
public func dfoo(_ x: Float, partial: Float, seed: Float) -> Float {
  return 2 * x
}

// CHECK-LABEL: sil @dfoo

