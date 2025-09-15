// RUN: %target-typecheck-verify-swift

struct G<T> {}
  
protocol P {
  associatedtype T
  associatedtype U
}

protocol Q: P where T == G<U> {}

protocol R: Q where U == Int {}

struct X: R {}
