// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/57019

class A { }
class B<T> : A { }
class C {
  func bar(x : (A?) -> Void) { }
}
class D<T> : C {
  override func bar(x : (B<T>?) -> Void) { }
}
