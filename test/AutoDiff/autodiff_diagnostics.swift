// RUN: %target-swift-frontend -emit-sil -verify %s

func foo(_ f: @escaping (Float) -> Float) -> Float {
  return #gradient(f)(0) // expected-error {{differentiating an opaque function is not supported yet}}
}
