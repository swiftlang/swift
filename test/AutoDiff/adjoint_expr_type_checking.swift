// RUN: %target-swift-frontend -typecheck -verify %s

// Note: the adjoint calculations in this file are not intended to be accurate.

//===----------------------------------------------------------------------===//
// Top-level function
//===----------------------------------------------------------------------===//

@differentiable(reverse, adjoint: dSquare(_:primal:seed:))
func square<T : FloatingPoint>(_ x: T) -> T { return x * x }
// expected-note @+1 {{in call to function 'dSquare(_:primal:seed:)'}}
private func dSquare<T : FloatingPoint>(_ x: T, primal: T, seed: T) -> T {
  return 2 * x
}
// Overload `square` with non-differentiable cases.
func square(_ x: Float) -> Float { return x * x }
func square(_ x: Double) -> Double { return x * x }

let _: (Float, Float, Float) -> Float = #adjoint(square)
func testTopLevel() -> Float {
  let adjointSquare: (Float, Float, Float) -> Float = #adjoint(square)
  return adjointSquare(1, 2, 3)
}
func testTopLevelGeneric<T : FloatingPoint>(_ x: T) -> T {
  let adjointSquare: (T, T, T) -> T = #adjoint(square)
  return adjointSquare(x, x, x)
}
func testTopLevel2() -> Float {
  return #adjoint(square)(1, primal: 2, seed: 3)
}
func testTopLevelGeneric2<T : FloatingPoint>(_ x: T) -> T {
  return #adjoint(square)(x, primal: x, seed: x)
}

//===----------------------------------------------------------------------===//
// Nested type
//===----------------------------------------------------------------------===//

struct A {
  struct B {
    struct C {
      @differentiable(reverse, adjoint: dAdd(a:b:primal:seed:))
      public static func add(a: C, b: C) -> C {
        return a
      }

      @usableFromInline internal static func dAdd(
        a: C, b: C, primal: C, seed: C
      ) -> (C, C) {
        return (seed, seed)
      }

      @differentiable(
        reverse, wrt: (self, .0),
        adjoint: dSubtract(a:primal:seed:)
      )
      func subtract(a: C) -> C {
        return a
      }

      private func dSubtract(
        a: C, primal: C, seed: C
      ) -> (C, C) {
        return (seed, seed)
      }
    }
  }
}

let _ = #adjoint(A.B.C.add)
let _ = #adjoint(A.B.C.subtract)
extension A {
  func test() {
    _ = #adjoint(B.C.add)
    _ = #adjoint(B.C.subtract)
  }
}
extension A.B {
  func test() {
    _ = #adjoint(C.add)
    _ = #adjoint(C.subtract)
  }
}
func testNested() {
  _ = #adjoint(A.B.C.add)
  _ = #adjoint(A.B.C.subtract)
}

//===----------------------------------------------------------------------===//
// Non-generic type
//===----------------------------------------------------------------------===//

struct Pair {
  let x: Float
  let y: Float
}
extension Pair {
  @differentiable(reverse, adjoint: dAdd(_:_:primal:seed:))
  static func + (_ a: Pair, _ b: Pair) -> Pair {
    return Pair(x: a.x+b.x, y: a.y+b.y)
  }

  private static func dAdd(
    _ a: Pair, _ b: Pair, primal: Pair, seed: Pair
  ) -> (Pair, Pair) {
    return (seed, seed)
  }

  @differentiable(reverse, wrt: (self, .0), adjoint: dSubtract(_:primal:seed:))
  func subtract(_ a: Pair) -> Pair {
    return Pair(x: x-a.x, y: y-a.y)
  }

  private func dSubtract(
    _ a: Pair, primal: Pair, seed: Pair
  ) -> (Pair, Pair) {
    return (seed, Pair(x: -seed.x, y: -seed.y))
  }
}

extension Pair {
  func testSameTypeContext() {
    _ = #adjoint(+)
    _ = #adjoint(subtract)
  }
}

func testPair() {
  _ = #adjoint(Pair.+)
  _ = #adjoint(Pair.+(_:_:))
  _ = #adjoint(Pair.subtract)
  _ = #adjoint(Pair.subtract(_:))
}

//===----------------------------------------------------------------------===//
// Generic type with generic functions
//===----------------------------------------------------------------------===//

struct Vector<T> {
  @differentiable(reverse, adjoint: dMultiply(_:_:primal:seed:))
  static func * <A : FloatingPoint>(
    _ a: Vector<A>, _ b: Vector<A>
  ) -> Vector<A> {
    return a
  }

  @differentiable(reverse, wrt: (self, .0), adjoint: dDivide(_:primal:seed:))
  func divide<A : FloatingPoint>(_ a: Vector<A>) -> Vector<A> {
    return a
  }

  static func dMultiply<A : FloatingPoint>(
    _ a: Vector<A>, _ b: Vector<A>, primal: Vector<A>, seed: Vector<A>
  ) -> (Vector<A>, Vector<A>) {
    return (seed, seed)
  }

  func dDivide<A : FloatingPoint>(
    _ a: Vector<A>, primal: Vector<A>, seed: Vector<A>
  ) -> (Vector, Vector<A>) {
    return (self, seed)
  }
}

extension Vector {
  static func dMultiply2<A : FloatingPoint>(
    _ a: Vector<A>, _ b: Vector<A>, primal: Vector<A>, seed: Vector<A>
  ) -> (Vector<A>, Vector<A>) {
    let adjoint: (Vector<A>, Vector<A>, Vector<A>, Vector<A>) -> (Vector<A>, Vector<A>)
      = #adjoint(*)
    return adjoint(a, b, primal, seed)
  }

  func dDivide2<A : FloatingPoint>(
    _ a: Vector<A>, primal: Vector<A>, seed: Vector<A>
  ) -> (Vector, Vector<A>) {
    let adjoint: (Vector) -> (Vector<A>, Vector<A>, Vector<A>) -> (Vector, Vector<A>) =
      #adjoint(divide)
    return adjoint(self)(a, primal, seed)
  }

  static func dMultiply3<A : FloatingPoint>(
    _ a: Vector<A>, _ b: Vector<A>, primal: Vector<A>, seed: Vector<A>
  ) -> (Vector<A>, Vector<A>) {
    return #adjoint(*(_:_:))(a, b, primal: primal, seed: seed)
  }

  func dDivide3<A : FloatingPoint>(
    _ a: Vector<A>, primal: Vector<A>, seed: Vector<A>
  ) -> (Vector, Vector<A>) {
    return #adjoint(divide(_:))(self)(a, primal: primal, seed: seed)
  }
}

let _: (Vector<Float>, Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector<Float>, Vector<Float>)
  = #adjoint(Vector<Any>.*)
let _: (Vector<Any>) -> (Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector<Any>, Vector<Float>)
  = #adjoint(Vector.divide)

extension Vector {
  func testSameTypeContext() {
    let _: (Vector<Float>, Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector<Float>, Vector<Float>)
      = #adjoint(*)
    let _: (Vector) -> (Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector, Vector<Float>)
      = #adjoint(divide)
  }
}

func testVector<T, A : FloatingPoint>(_ t: T, _ a: A) {
  let _: (Vector<A>, Vector<A>, Vector<A>, Vector<A>) -> (Vector<A>, Vector<A>)
    = #adjoint(Vector<T>.*)
  let _: (Vector<T>) -> (Vector<A>, Vector<A>, Vector<A>) -> (Vector<T>, Vector<A>)
    = #adjoint(Vector<T>.divide)
}

//===----------------------------------------------------------------------===//
// Errors
//===----------------------------------------------------------------------===//

let _ = #adjoint(square) // expected-error {{generic parameter 'T' could not be inferred}}
let _ = dSquare // expected-error {{generic parameter 'T' could not be inferred}}

// Note: I think the case below is theoretically resolvable by the type checker.
// The failing behavior is consistent for both #adjoint and direct adjoint
// reference, so things are good.

// expected-error @+2 {{cannot convert value of type '(Vector<_>, Vector<_>, Vector<_>, Vector<_>) -> (Vector<_>, Vector<_>)' to specified type '(Vector<Float>, Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector<Float>, Vector<Float>)'}}
let _: (Vector<Float>, Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector<Float>, Vector<Float>)
  = #adjoint(Vector.*)
// expected-error @+2 {{cannot convert value of type '(Vector<_>, Vector<_>, Vector<_>, Vector<_>) -> (Vector<_>, Vector<_>)' to specified type '(Vector<Float>, Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector<Float>, Vector<Float>)'}}
let _: (Vector<Float>, Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector<Float>, Vector<Float>)
  = Vector.dMultiply
