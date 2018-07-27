// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -Xllvm -tf-strict-deabstraction -verify %s
import TensorFlow

public func SR8299(a: Tensor<Float>) {
   // expected-error @+1 {{attribute 'someAttr' requires a constant argument}}
   () = #tfop("foo", someAttr: a)
}
