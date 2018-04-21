// RUN: %target-swift-frontend -O -emit-sil -verify %s

func foo(_ f: @escaping (Float) -> Float) -> Float {
  return #gradient(of: f)(0) // expected-error {{differentiating an opaque function is not supported yet}}
}
