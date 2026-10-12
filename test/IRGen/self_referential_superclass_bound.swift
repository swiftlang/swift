// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -O %s

// Ensure that self-referential superclass bounds won't cause an infinite
// recursion crash.

class Base<T> {}

protocol Q: Base<Self> {}

extension Q {
  // CHECK: define{{.*}} @"$s33self_referential_superclass_bound1QPAAE1fyyFZ"
  static func f() {}
}

// Ensure that a similar infinite recursion doesn't happen through an associated
// type with a class bound that is equated back to Self via a same-type
// requirement.
class a<b> {
}
protocol c {
  associatedtype f: a<d>
  associatedtype d
}
extension c where Self == f, f == d {
  // CHECK: define{{.*}} @"$s33self_referential_superclass_bound1cPAA1dQzRsz1fQzAERSrlE1eyyF"
  func e() {
  }
}
