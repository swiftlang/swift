// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo -o /dev/null

// REQUIRES: swift_feature_Embedded

// Infinitely growing specializations must be diagnosed instead of hanging the
// compiler. The exponential cases test that the compiler stops quickly when
// each specialization creates multiple growing specializations.

// VTables

// Specializing the vtable of `Box<Int>` specializes `wrap`, which allocates a
// `Box<Optional<Int>>`, whose vtable must be specialized, and so on.

public class Box<T> {
  var t: T

  init(_ t: T) {
    self.t = t
  }

  func wrap() -> AnyObject {
    return Box<Optional<T>>(t) // expected-error {{cannot specialize 'Box<Int>' in embedded Swift because its methods create infinitely many specializations of the class}}
  }
}

// Specializations with growing, but a finite number of types are fine.
public class Node<T> {
  var t: T

  init(_ t: T) {
    self.t = t
  }

  func copy() -> AnyObject {
    return Node<T>(t)
  }
}

public func testInfinite() -> AnyObject {
  return Box<Int>(1)
}

public func testFinite() -> [AnyObject] {
  return [Node<Int>(1), Node<[Int]>([1]), Node<[[Int]]>([[1]]), Node<[[[Int]]]>([[[1]]])]
}

// Witness tables

public protocol P {
  func next() -> [any P]
}

struct S<T>: P {
  var t: T

  func next() -> [any P] {
    return [S<Optional<T>>(t: t)] // expected-error {{cannot specialize the conformance of 'S<Int>' to 'P' in embedded Swift because its witness methods create infinitely many specializations of the conformance}}
  }
}

// Exponential growth: each specialization creates three growing specializations.
enum E<T>: P {
  case a

  func next() -> [any P] {
    return [E<Optional<T>>.a, E<[T]>.a, E<(T, T)>.a] // expected-error {{cannot specialize the conformance of 'E<Int>' to 'P' in embedded Swift because its witness methods create infinitely many specializations of the conformance}}
  }
}

public func testWitnessTable() -> any P {
  return S<Int>(t: 1)
}

public func testWitnessTableExponential() -> any P {
  return E<Int>.a
}

// Functions

@inline(never)
func growing<T>(_ x: T, _ n: Int) {
  if n > 0 {
    growing(Optional(x), n - 1) // expected-error {{cannot specialize generic function or default protocol method in this context}}
    growing([x], n - 1)
    growing((x, x), n - 1)
  }
}

public func testFunctionExponential() {
  growing(1, 5) // expected-note {{generic specialization called here}}
}

// Deinits of non-copyable types

struct NC<T>: ~Copyable {
  var t: T

  deinit {
    if Int.random(in: 0..<2) == 0 { _ = NC<Optional<T>>(t: t) } // expected-error {{cannot specialize generic function or default protocol method in this context}}
    if Int.random(in: 0..<2) == 0 { _ = NC<[T]>(t: [t]) }
    if Int.random(in: 0..<2) == 0 { _ = NC<(T, T)>(t: (t, t)) }
  }
}

public func testDeinitExponential() {
  _ = NC<Int>(t: 1) // expected-note {{generic specialization called here}}
}
