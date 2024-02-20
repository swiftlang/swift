// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: not %target-typecheck-verify-swift -disable-experimental-associated-type-inference

struct G<T> {}
  
protocol P {
  associatedtype T
  associatedtype U
}

protocol Q: P where T == G<U> {}

protocol R: Q where U == Int {}

struct X: R {}
