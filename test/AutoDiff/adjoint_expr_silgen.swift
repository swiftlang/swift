// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

// Note: the adjoint calculations in this file are not intended to be accurate.

//===----------------------------------------------------------------------===//
// Top-level function
//===----------------------------------------------------------------------===//

@differentiable(reverse, adjoint: dSquare)
func square<T : FloatingPoint>(_ x: T) -> T {
  return x * x
}
@_silgen_name("dsquare")
private func dSquare<T : FloatingPoint>(_ seed: T, _ primal: T, _ x: T) -> T {
  return 2 * x
}

@_silgen_name("test_top_level")
func testTopLevel() -> Float {
  let adjointSquare: (Float, Float, Float) -> Float = #adjoint(square)
  return adjointSquare(1, 2, 3)
}
// CHECK-LABEL: sil hidden @test_top_level
// CHECK: [[DSQUARE_RAW:%.*]] = function_ref @dsquare : $@convention(thin) <τ_0_0 where τ_0_0 : FloatingPoint> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK: {{%.*}} = partial_apply [callee_guaranteed] [[DSQUARE_RAW]]<Float>

@_silgen_name("test_top_level_generic")
func testTopLevelGeneric<T : FloatingPoint>(_ x: T) -> T {
  let adjointSquare: (T, T, T) -> T = #adjoint(square)
  return adjointSquare(x, x, x)
}
// CHECK-LABEL: sil hidden @test_top_level_generic : $@convention(thin) <T where T : FloatingPoint> (@in_guaranteed T) -> @out T
// CHECK: [[DSQUARE_RAW_2:%.*]] = function_ref @dsquare : $@convention(thin) <τ_0_0 where τ_0_0 : FloatingPoint> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK: {{%.*}} = partial_apply [callee_guaranteed] [[DSQUARE_RAW_2]]<T>()

//===----------------------------------------------------------------------===//
// Nested type
//===----------------------------------------------------------------------===//

struct A {
  struct B {
    struct C {
      @differentiable(reverse, adjoint: dAdd(seed:primal:a:b:))
      public static func add(a: C, b: C) -> C {
        return a
      }

      @usableFromInline internal static func dAdd(
        seed: C, primal: C, a: C, b: C
      ) -> (C, C) {
        return (seed, seed)
      }

      @differentiable(
        reverse, wrt: (self, .0),
        adjoint: dSubtract(seed:primal:a:)
      )
      func subtract(a: C) -> C {
        return a
      }

      private func dSubtract(
        seed: C, primal: C, a: C
      ) -> (C, C) {
        return (seed, seed)
      }
    }
  }
}

@_silgen_name("test_nested")
func testNested() {
  _ = #adjoint(A.B.C.add)
  _ = #adjoint(A.B.C.subtract)
}
// CHECK-LABEL: sil hidden @test_nested
// CHECK: [[NESTED_DADD_THUNK:%.*]] = function_ref {{.*}} : $@convention(method) (A.B.C, A.B.C, A.B.C, A.B.C, @thin A.B.C.Type) -> (A.B.C, A.B.C)
// CHECK: [[NESTED_META:%.*]] = metatype $@thin A.B.C.Type
// CHECK: {{%.*}} = partial_apply [callee_guaranteed] [[NESTED_DADD_THUNK]]([[NESTED_META]])

// CHECK: [[NESTED_DSUB_THUNK:%.*]] = function_ref {{.*}} : $@convention(thin) (A.B.C) -> @owned @callee_guaranteed (A.B.C, A.B.C, A.B.C) -> (A.B.C, A.B.C)
// CHECK: {{%.*}} = thin_to_thick_function [[NESTED_DSUB_THUNK]]

//===----------------------------------------------------------------------===//
// Non-generic type
//===----------------------------------------------------------------------===//

struct Pair {
  let x: Float
  let y: Float
}
extension Pair {
  @differentiable(reverse, adjoint: dAdd)
  static func + (_ a: Pair, _ b: Pair) -> Pair {
    return Pair(x: a.x+b.x, y: a.y+b.y)
  }

  private static func dAdd(
    _ seed: Pair, _ primal: Pair, _ a: Pair, _ b: Pair
  ) -> (Pair, Pair) {
    return (seed, seed)
  }

  @differentiable(reverse, wrt: (self, .0), adjoint: dSubtract)
  func subtract(_ a: Pair) -> Pair {
    return Pair(x: x-a.x, y: y-a.y)
  }

  private func dSubtract(
    _ seed: Pair, _ primal: Pair, _ a: Pair
  ) -> (Pair, Pair) {
    return (seed, Pair(x: -seed.x, y: -seed.y))
  }
}

extension Pair {
  @_silgen_name("test_pair_extension")
  func testSameTypeContext() {
    _ = #adjoint(+)
    _ = #adjoint(subtract)
  }
}
// CHECK-LABEL: sil hidden @test_pair_extension
// CHECK: [[PAIR_DADD_THUNK:%.*]] = function_ref {{.*}} : $@convention(method) (Pair, Pair, Pair, Pair, @thin Pair.Type) -> (Pair, Pair)
// CHECK: [[PAIR_META:%.*]] = metatype $@thin Pair.Type
// CHECK: {{%.*}} = partial_apply [callee_guaranteed] [[PAIR_DADD_THUNK]]([[PAIR_META]])

// CHECK: [[PAIR_DSUB_THUNK:%.*]] = function_ref {{.*}} : $@convention(thin) (Pair) -> @owned @callee_guaranteed (Pair, Pair, Pair) -> (Pair, Pair)
// CHECK: {{%.*}} = thin_to_thick_function [[PAIR_DSUB_THUNK]]

@_silgen_name("test_pair")
func testPair() {
  _ = #adjoint(Pair.+)
  _ = #adjoint(Pair.subtract)
}
// CHECK-LABEL: sil hidden @test_pair
// CHECK: [[PAIR_DADD_THUNK_2:%.*]] = function_ref {{.*}} : $@convention(method) (Pair, Pair, Pair, Pair, @thin Pair.Type) -> (Pair, Pair)
// CHECK: [[PAIR_META_2:%.*]] = metatype $@thin Pair.Type
// CHECK: {{%.*}} = partial_apply [callee_guaranteed] [[PAIR_DADD_THUNK_2]]([[PAIR_META_2]])

// CHECK: [[PAIR_DSUB_THUNK_2:%.*]] = function_ref {{.*}} : $@convention(thin) (Pair) -> @owned @callee_guaranteed (Pair, Pair, Pair) -> (Pair, Pair)
// CHECK: {{%.*}} = thin_to_thick_function [[PAIR_DSUB_THUNK_2]]

//===----------------------------------------------------------------------===//
// Generic type with generic functions
//===----------------------------------------------------------------------===//

struct Vector<T> {
  @differentiable(reverse, adjoint: dMultiply)
  static func * <A : FloatingPoint>(
    _ a: Vector<A>, _ b: Vector<A>
  ) -> Vector<A> {
    return a
  }

  @differentiable(reverse, wrt: (self, .0), adjoint: dDivide)
  func divide<A : FloatingPoint>(_ a: Vector<A>) -> Vector<A> {
    return a
  }

  static func dMultiply<A : FloatingPoint>(
    _ seed: Vector<A>, _ primal: Vector<A>, _ a: Vector<A>, _ b: Vector<A>
  ) -> (Vector<A>, Vector<A>) {
    return (seed, seed)
  }

  func dDivide<A : FloatingPoint>(
    _ seed: Vector<A>, _ primal: Vector<A>, _ a: Vector<A>
  ) -> (Vector, Vector<A>) {
    return (self, seed)
  }
}

extension Vector {
  @_silgen_name("test_vector_extension")
  func testSameTypeContext() {
    let _: (Vector<Float>, Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector<Float>, Vector<Float>)
      = #adjoint(*)
    let _: (Vector) -> (Vector<Float>, Vector<Float>, Vector<Float>) -> (Vector, Vector<Float>)
      = #adjoint(divide)
  }
}
// CHECK-LABEL: sil hidden @test_vector_extension : $@convention(method) <T> (Vector<T>) -> ()
// CHECK: [[VEC_DMUL_THUNK:%.*]] = function_ref {{.*}} : $@convention(method) <τ_0_0><τ_1_0 where τ_1_0 : FloatingPoint> (Vector<τ_1_0>, Vector<τ_1_0>, Vector<τ_1_0>, Vector<τ_1_0>, @thin Vector<τ_0_0>.Type) -> (Vector<τ_1_0>, Vector<τ_1_0>)
// CHECK: [[VEC_DMUL_META:%.*]] = metatype $@thin Vector<T>.Type
// CHECK: {{%.*}} = partial_apply [callee_guaranteed] [[VEC_DMUL_THUNK]]<T, Float>([[VEC_DMUL_META]])

// CHECK: [[VEC_DDIV_THUNK:%.*]] = function_ref {{.*}} : $@convention(thin) <τ_0_0><τ_1_0 where τ_1_0 : FloatingPoint> (Vector<τ_0_0>) -> @owned @callee_guaranteed (Vector<τ_1_0>, Vector<τ_1_0>, Vector<τ_1_0>) -> (Vector<τ_0_0>, Vector<τ_1_0>)
// CHECK: {{%.*}} = partial_apply [callee_guaranteed] [[VEC_DDIV_THUNK]]<T, Float>()

@_silgen_name("test_vector")
func testVector<T, A : FloatingPoint>(_ t: T, _ a: A) {
  let _: (Vector<A>, Vector<A>, Vector<A>, Vector<A>) -> (Vector<A>, Vector<A>)
    = #adjoint(Vector<T>.*)
  let _: (Vector<T>) -> (Vector<A>, Vector<A>, Vector<A>) -> (Vector<T>, Vector<A>)
    = #adjoint(Vector<T>.divide)
}
// CHECK-LABEL: sil hidden @test_vector : $@convention(thin) <T, A where A : FloatingPoint> (@in_guaranteed T, @in_guaranteed A) -> ()
// CHECK: [[VEC_DMUL_THUNK_2:%.*]] = function_ref {{.*}} : $@convention(method) <τ_0_0><τ_1_0 where τ_1_0 : FloatingPoint> (Vector<τ_1_0>, Vector<τ_1_0>, Vector<τ_1_0>, Vector<τ_1_0>, @thin Vector<τ_0_0>.Type) -> (Vector<τ_1_0>, Vector<τ_1_0>)
// CHECK: [[VEC_DMUL_META_2:%.*]] = metatype $@thin Vector<T>.Type
// CHECK: {{%.*}} = partial_apply [callee_guaranteed] [[VEC_DMUL_THUNK_2]]<T, A>([[VEC_DMUL_META_2]])

// CHECK: [[VEC_DDIV_THUNK_2:%.*]] = function_ref {{.*}} : $@convention(thin) <τ_0_0><τ_1_0 where τ_1_0 : FloatingPoint> (Vector<τ_0_0>) -> @owned @callee_guaranteed (Vector<τ_1_0>, Vector<τ_1_0>, Vector<τ_1_0>) -> (Vector<τ_0_0>, Vector<τ_1_0>)
// CHECK: {{%.*}} = partial_apply [callee_guaranteed] [[VEC_DDIV_THUNK_2]]<T, A>()
