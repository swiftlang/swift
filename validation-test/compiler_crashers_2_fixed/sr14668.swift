// RUN: %target-swift-frontend -emit-ir %s

class A { }
class B<T> : A { }
class C {
  func bar(x : (A?) -> Void) { }
}
class D<T> : C {
  override func bar(x : (B<T>?) -> Void) { }
}
