// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -swift-version 5 -wmo -o /dev/null

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// We don't support calling a default witness method (which is generic) with dynamic self.

public protocol P {
  init(x: Int)
}

extension P {
  init(y: Int) {
    self.init(x: y)
  }
}

public class C: P {
  public required init(x: Int) {}

  public convenience init(z: Int) {
    self.init(y: z)  // expected-error {{cannot call an initializer or static method, which is defined as default protocol method, from a class method or initializer}}
  }
}

struct S<T> {
  let t: T
}

func infiniteSpecialization<T>(t: T, i : Int) {
  if i > 0 {
    infiniteSpecialization(t: S(t: t), i: i - 1) // expected-error {{cannot specialize generic function or default protocol method in this context}}
  }
}

public func callInfiniteSpecialization() {
  infiniteSpecialization(t: S(t: 1), i: 10) // expected-note {{generic specialization called here}}
}

protocol P2 {
  associatedtype A : P2

  var a: A { get }
}

struct S3 : P2 {
  let a: S2
}

struct S2 : P2 {
  let a: S1
}

struct S1 : P2 {
  var a: S1 { return self }
}

func finiteSpecialization<T: P2>(t: T, i : Int) {
  if i > 0 {
    finiteSpecialization(t: t.a, i: i - 1)
  }
}

public func callFiniteSpecialization() {
  finiteSpecialization(t: S3(a: S2(a: S1())), i: 10)
}
