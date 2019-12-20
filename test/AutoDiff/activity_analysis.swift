// RUN: %target-swift-emit-sil -verify -Xllvm -debug-only=differentiation 2>&1 %s | %FileCheck %s
// REQUIRES: asserts

// Check that `@noDerivative` struct projections have "NONE" activity.

struct HasNoDerivativeProperty: Differentiable {
  var x: Float
  @noDerivative var y: Float
}
@differentiable
func testNoDerivativeStructProjection(_ s: HasNoDerivativeProperty) -> Float {
  var tmp = s
  tmp.y = tmp.x
  return tmp.x
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}testNoDerivativeStructProjection{{.*}} at (source=0 parameters=(0))
// CHECK: [ACTIVE] %0 = argument of bb0 : $HasNoDerivativeProperty
// CHECK: [ACTIVE]   %2 = alloc_stack $HasNoDerivativeProperty, var, name "tmp"
// CHECK: [ACTIVE]   %4 = begin_access [read] [static] %2 : $*HasNoDerivativeProperty
// CHECK: [ACTIVE]   %5 = struct_element_addr %4 : $*HasNoDerivativeProperty, #HasNoDerivativeProperty.x
// CHECK: [VARIED]   %6 = load [trivial] %5 : $*Float
// CHECK: [ACTIVE]   %8 = begin_access [modify] [static] %2 : $*HasNoDerivativeProperty
// CHECK: [NONE]   %9 = struct_element_addr %8 : $*HasNoDerivativeProperty, #HasNoDerivativeProperty.y
// CHECK: [ACTIVE]   %12 = begin_access [read] [static] %2 : $*HasNoDerivativeProperty
// CHECK: [ACTIVE]   %13 = struct_element_addr %12 : $*HasNoDerivativeProperty, #HasNoDerivativeProperty.x
// CHECK: [ACTIVE]   %14 = load [trivial] %13 : $*Float

// Check that non-differentiable `tuple_element_addr` projections are non-varied.

@differentiable(where T : Differentiable)
func testNondifferentiableTupleElementAddr<T>(_ x: T) -> T {
  var tuple = (1, 1, (x, 1), 1)
  tuple.0 = 1
  tuple.2.0 = x
  tuple.3 = 1
  return tuple.2.0
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}testNondifferentiableTupleElementAddr{{.*}} at (source=0 parameters=(0))
// CHECK: [ACTIVE] %0 = argument of bb0 : $*T
// CHECK: [ACTIVE] %1 = argument of bb0 : $*T
// CHECK: [ACTIVE]   %3 = alloc_stack $(Int, Int, (T, Int), Int), var, name "tuple"
// CHECK: [USEFUL]   %4 = tuple_element_addr %3 : $*(Int, Int, (T, Int), Int), 0
// CHECK: [USEFUL]   %5 = tuple_element_addr %3 : $*(Int, Int, (T, Int), Int), 1
// CHECK: [ACTIVE]   %6 = tuple_element_addr %3 : $*(Int, Int, (T, Int), Int), 2
// CHECK: [USEFUL]   %7 = tuple_element_addr %3 : $*(Int, Int, (T, Int), Int), 3
// CHECK: [ACTIVE]   %18 = tuple_element_addr %6 : $*(T, Int), 0
// CHECK: [USEFUL]   %19 = tuple_element_addr %6 : $*(T, Int), 1
// CHECK: [ACTIVE]   %35 = begin_access [modify] [static] %3 : $*(Int, Int, (T, Int), Int)
// CHECK: [USEFUL]   %36 = tuple_element_addr %35 : $*(Int, Int, (T, Int), Int), 0
// CHECK: [ACTIVE]   %41 = begin_access [modify] [static] %3 : $*(Int, Int, (T, Int), Int)
// CHECK: [ACTIVE]   %42 = tuple_element_addr %41 : $*(Int, Int, (T, Int), Int), 2
// CHECK: [ACTIVE]   %43 = tuple_element_addr %42 : $*(T, Int), 0
// CHECK: [ACTIVE]   %51 = begin_access [modify] [static] %3 : $*(Int, Int, (T, Int), Int)
// CHECK: [USEFUL]   %52 = tuple_element_addr %51 : $*(Int, Int, (T, Int), Int), 3
// CHECK: [ACTIVE]   %55 = begin_access [read] [static] %3 : $*(Int, Int, (T, Int), Int)
// CHECK: [ACTIVE]   %56 = tuple_element_addr %55 : $*(Int, Int, (T, Int), Int), 2
// CHECK: [ACTIVE]   %57 = tuple_element_addr %56 : $*(T, Int), 0

// TF-781: check active local address + nested conditionals.

@differentiable(wrt: x)
func TF_781(_ x: Float, _ y: Float) -> Float {
  var result = y
  if true {
    if true {
      result = result * x // check activity of `result` and this `apply`
    }
  }
  return result
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}TF_781{{.*}} at (source=0 parameters=(0))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [USEFUL] %1 = argument of bb0 : $Float
// CHECK: [ACTIVE]   %4 = alloc_stack $Float, var, name "result"
// CHECK: [ACTIVE]   %19 = begin_access [read] [static] %4 : $*Float
// CHECK: [ACTIVE]   %20 = load [trivial] %19 : $*Float
// CHECK: [ACTIVE]   %23 = apply %22(%20, %0, %18) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK: [ACTIVE]   %24 = begin_access [modify] [static] %4 : $*Float
// CHECK: [ACTIVE]   %31 = begin_access [read] [static] %4 : $*Float
// CHECK: [ACTIVE]   %32 = load [trivial] %31 : $*Float

// TF-954: check nested conditionals and addresses.

@differentiable
func TF_954(_ x: Float) -> Float {
  var outer = x
  outerIf: if true {
    var inner = outer
    inner = inner * x // check activity of this `apply`
    if false {
      break outerIf
    }
    outer = inner
  }
  return outer
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}TF_954{{.*}} at (source=0 parameters=(0))
// CHECK: bb0:
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [ACTIVE]   %2 = alloc_stack $Float, var, name "outer"
// CHECK: bb1:
// CHECK: [ACTIVE]   %10 = alloc_stack $Float, var, name "inner"
// CHECK: [ACTIVE]   %11 = begin_access [read] [static] %2 : $*Float
// CHECK: [USEFUL]   %14 = metatype $@thin Float.Type
// CHECK: [ACTIVE]   %15 = begin_access [read] [static] %10 : $*Float
// CHECK: [ACTIVE]   %16 = load [trivial] %15 : $*Float
// CHECK: [NONE]   // function_ref static Float.* infix(_:_:)
// CHECK:   %18 = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK: [ACTIVE]   %19 = apply %18(%16, %0, %14) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK: [ACTIVE]   %20 = begin_access [modify] [static] %10 : $*Float
// CHECK: bb3:
// CHECK: [ACTIVE]   %31 = begin_access [read] [static] %10 : $*Float
// CHECK: [ACTIVE]   %32 = load [trivial] %31 : $*Float
// CHECK: [ACTIVE]   %34 = begin_access [modify] [static] %2 : $*Float
// CHECK: bb5:
// CHECK: [ACTIVE]   %40 = begin_access [read] [static] %2 : $*Float
// CHECK: [ACTIVE]   %41 = load [trivial] %40 : $*Float

//===----------------------------------------------------------------------===//
// Array literal differentiation
//===----------------------------------------------------------------------===//

// Check `array.uninitialized_intrinsic` applications.

@differentiable
func testArrayUninitializedIntrinsic(_ x: Float, _ y: Float) -> [Float] {
  return [x, y]
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}testArrayUninitializedIntrinsic{{.*}} at (source=0 parameters=(0 1))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [ACTIVE] %1 = argument of bb0 : $Float
// CHECK: [USEFUL]   %4 = integer_literal $Builtin.Word, 2
// CHECK: [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [ACTIVE]   %6 = apply %5<Float>(%4) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [ACTIVE] (**%7**, %8) = destructure_tuple %6 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [VARIED] (%7, **%8**) = destructure_tuple %6 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [ACTIVE]   %9 = pointer_to_address %8 : $Builtin.RawPointer to [strict] $*Float
// CHECK: [VARIED]   %11 = integer_literal $Builtin.Word, 1
// CHECK: [ACTIVE]   %12 = index_addr %9 : $*Float, %11 : $Builtin.Word

@differentiable(where T: Differentiable)
func testArrayUninitializedIntrinsicGeneric<T>(_ x: T, _ y: T) -> [T] {
  return [x, y]
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}testArrayUninitializedIntrinsicGeneric{{.*}} at (source=0 parameters=(0 1))
// CHECK: [ACTIVE] %0 = argument of bb0 : $*T
// CHECK: [ACTIVE] %1 = argument of bb0 : $*T
// CHECK: [USEFUL]   %4 = integer_literal $Builtin.Word, 2
// CHECK: [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [ACTIVE]   %6 = apply %5<T>(%4) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [ACTIVE] (**%7**, %8) = destructure_tuple %6 : $(Array<T>, Builtin.RawPointer)
// CHECK: [VARIED] (%7, **%8**) = destructure_tuple %6 : $(Array<T>, Builtin.RawPointer)
// CHECK: [ACTIVE]   %9 = pointer_to_address %8 : $Builtin.RawPointer to [strict] $*T
// CHECK: [VARIED]   %11 = integer_literal $Builtin.Word, 1
// CHECK: [ACTIVE]   %12 = index_addr %9 : $*T, %11 : $Builtin.Word

// TF-952: Test array literal initialized from an address (e.g. `var`).
@differentiable
func testArrayUninitializedIntrinsicAddress(_ x: Float, _ y: Float) -> [Float] {
  var result = x
  result = result * y
  return [result, result]
}
// CHECK-LABEL: [AD] Activity info for ${{.*}}testArrayUninitializedIntrinsicAddress{{.*}} at (source=0 parameters=(0 1))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [ACTIVE] %1 = argument of bb0 : $Float
// CHECK: [ACTIVE]   %4 = alloc_stack $Float, var, name "result"
// CHECK: [ACTIVE]   %7 = begin_access [read] [static] %4 : $*Float
// CHECK: [ACTIVE]   %8 = load [trivial] %7 : $*Float
// CHECK: [NONE]   // function_ref static Float.* infix(_:_:)
// CHECK: [ACTIVE]   %11 = apply %10(%8, %1, %6) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK: [ACTIVE]   %12 = begin_access [modify] [static] %4 : $*Float
// CHECK: [USEFUL]   %15 = integer_literal $Builtin.Word, 2
// CHECK: [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [ACTIVE]   %17 = apply %16<Float>(%15) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [ACTIVE] (**%18**, %19) = destructure_tuple %17 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [VARIED] (%18, **%19**) = destructure_tuple %17 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [ACTIVE]   %20 = pointer_to_address %19 : $Builtin.RawPointer to [strict] $*Float
// CHECK: [ACTIVE]   %21 = begin_access [read] [static] %4 : $*Float
// CHECK: [VARIED]   %24 = integer_literal $Builtin.Word, 1
// CHECK: [ACTIVE]   %25 = index_addr %20 : $*Float, %24 : $Builtin.Word
// CHECK: [ACTIVE]   %26 = begin_access [read] [static] %4 : $*Float

// TF-952: Test array literal initialized with `apply` direct results.
@differentiable
func testArrayUninitializedIntrinsicFunctionResult(_ x: Float, _ y: Float) -> [Float] {
  return [x * y, x * y]
}
// CHECK-LABEL: [AD] Activity info for ${{.*}}testArrayUninitializedIntrinsicFunctionResult{{.*}} at (source=0 parameters=(0 1))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [ACTIVE] %1 = argument of bb0 : $Float
// CHECK: [USEFUL]   %4 = integer_literal $Builtin.Word, 2
// CHECK: [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [ACTIVE]   %6 = apply %5<Float>(%4) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [ACTIVE] (**%7**, %8) = destructure_tuple %6 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [VARIED] (%7, **%8**) = destructure_tuple %6 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [ACTIVE]   %9 = pointer_to_address %8 : $Builtin.RawPointer to [strict] $*Float
// CHECK: [NONE]   // function_ref static Float.* infix(_:_:)
// CHECK: [ACTIVE]   %12 = apply %11(%0, %1, %10) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK: [VARIED]   %14 = integer_literal $Builtin.Word, 1
// CHECK: [ACTIVE]   %15 = index_addr %9 : $*Float, %14 : $Builtin.Word
// CHECK: [USEFUL]   %16 = metatype $@thin Float.Type
// CHECK: [NONE]   // function_ref static Float.* infix(_:_:)
// CHECK: [ACTIVE]   %18 = apply %17(%0, %1, %16) : $@convention(method) (Float, Float, @thin Float.Type) -> Float

// TF-975: Test nested array literals.
// FIXME(TF-975): Some values are incorrectly not marked as active: `%0`, `%1`, etc.
@differentiable
func testArrayUninitializedIntrinsicNested(_ x: Float, _ y: Float) -> [Float] {
  let array = [x, y]
  return [array[0], array[1]]
}
// CHECK-LABEL: [AD] Activity info for ${{.*}}testArrayUninitializedIntrinsicNested{{.*}} at (source=0 parameters=(0 1))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [ACTIVE] %1 = argument of bb0 : $Float
// CHECK: [USEFUL]   %4 = integer_literal $Builtin.Word, 2
// CHECK: [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [ACTIVE]   %6 = apply %5<Float>(%4) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [ACTIVE] (**%7**, %8) = destructure_tuple %6 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [VARIED] (%7, **%8**) = destructure_tuple %6 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [ACTIVE]   %9 = pointer_to_address %8 : $Builtin.RawPointer to [strict] $*Float
// CHECK: [VARIED]   %11 = integer_literal $Builtin.Word, 1
// CHECK: [ACTIVE]   %12 = index_addr %9 : $*Float, %11 : $Builtin.Word
// CHECK: [USEFUL]   %15 = integer_literal $Builtin.Word, 2
// CHECK: [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [ACTIVE]   %17 = apply %16<Float>(%15) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [ACTIVE] (**%18**, %19) = destructure_tuple %17 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [VARIED] (%18, **%19**) = destructure_tuple %17 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [ACTIVE]   %20 = pointer_to_address %19 : $Builtin.RawPointer to [strict] $*Float
// CHECK: [ACTIVE]   %21 = begin_borrow %7 : $Array<Float>
// CHECK: [USEFUL]   %22 = integer_literal $Builtin.IntLiteral, 0
// CHECK: [USEFUL]   %23 = metatype $@thin Int.Type
// CHECK: [NONE]   // function_ref Int.init(_builtinIntegerLiteral:)
// CHECK: [USEFUL]   %25 = apply %24(%22, %23) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [NONE]   // function_ref Array.subscript.getter
// CHECK: [NONE]   %27 = apply %26<Float>(%20, %25, %21) : $@convention(method) <τ_0_0> (Int, @guaranteed Array<τ_0_0>) -> @out τ_0_0
// CHECK: [VARIED]   %28 = integer_literal $Builtin.Word, 1
// CHECK: [ACTIVE]   %29 = index_addr %20 : $*Float, %28 : $Builtin.Word
// CHECK: [ACTIVE]   %30 = begin_borrow %7 : $Array<Float>
// CHECK: [USEFUL]   %31 = integer_literal $Builtin.IntLiteral, 1
// CHECK: [USEFUL]   %32 = metatype $@thin Int.Type
// CHECK: [NONE]   // function_ref Int.init(_builtinIntegerLiteral:)
// CHECK: [USEFUL]   %34 = apply %33(%31, %32) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [NONE]   // function_ref Array.subscript.getter
// CHECK: [NONE]   %36 = apply %35<Float>(%29, %34, %30) : $@convention(method) <τ_0_0> (Int, @guaranteed Array<τ_0_0>) -> @out τ_0_0

// TF-978: Test array literal initialized with `apply` indirect results.
// FIXME(TF-978): Some values are incorrectly not marked as active: `%0`, `%1`, etc.
struct Wrapper<T: Differentiable>: Differentiable {
  var value: T
}
@differentiable
func testArrayUninitializedIntrinsicApplyIndirectResult<T>(_ x: T, _ y: T) -> [Wrapper<T>] {
  return [Wrapper(value: x), Wrapper(value: y)]
}
// CHECK-LABEL: [AD] Activity info for ${{.*}}testArrayUninitializedIntrinsicApplyIndirectResult{{.*}} at (source=0 parameters=(0 1))
// CHECK: [ACTIVE] %0 = argument of bb0 : $*T
// CHECK: [ACTIVE] %1 = argument of bb0 : $*T
// CHECK: [USEFUL]   %4 = integer_literal $Builtin.Word, 2
// CHECK: [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [ACTIVE]   %6 = apply %5<Wrapper<T>>(%4) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [ACTIVE] (**%7**, %8) = destructure_tuple %6 : $(Array<Wrapper<T>>, Builtin.RawPointer)
// CHECK: [VARIED] (%7, **%8**) = destructure_tuple %6 : $(Array<Wrapper<T>>, Builtin.RawPointer)
// CHECK: [ACTIVE]   %9 = pointer_to_address %8 : $Builtin.RawPointer to [strict] $*Wrapper<T>
// CHECK: [USEFUL]   %10 = metatype $@thin Wrapper<T>.Type
// CHECK: [ACTIVE]   %11 = alloc_stack $T
// CHECK: [NONE]   // function_ref Wrapper.init(value:)
// CHECK: [NONE]   %14 = apply %13<T>(%9, %11, %10) : $@convention(method) <τ_0_0 where τ_0_0 : Differentiable> (@in τ_0_0, @thin Wrapper<τ_0_0>.Type) -> @out Wrapper<τ_0_0>
// CHECK: [VARIED]   %16 = integer_literal $Builtin.Word, 1
// CHECK: [ACTIVE]   %17 = index_addr %9 : $*Wrapper<T>, %16 : $Builtin.Word
// CHECK: [USEFUL]   %18 = metatype $@thin Wrapper<T>.Type
// CHECK: [ACTIVE]   %19 = alloc_stack $T
// CHECK: [NONE]   // function_ref Wrapper.init(value:)
// CHECK: [NONE]   %22 = apply %21<T>(%17, %19, %18) : $@convention(method) <τ_0_0 where τ_0_0 : Differentiable> (@in τ_0_0, @thin Wrapper<τ_0_0>.Type) -> @out Wrapper<τ_0_0>

//===----------------------------------------------------------------------===//
// `inout` argument differentiation
//===----------------------------------------------------------------------===//

struct Mut: Differentiable {}
extension Mut {
  @differentiable(wrt: x)
  mutating func mutatingMethod(_ x: Mut) -> Mut {
    return x
  }
}

// CHECK-LABEL: [AD] Activity info for $s17activity_analysis3MutV14mutatingMethodyA2CF at (source=0 parameters=(0))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Mut
// CHECK: [NONE] %1 = argument of bb0 : $*Mut

// TODO(TF-985): Find workaround to avoid marking non-wrt `inout` argument as
// active.
// expected-error @+1 {{function is not differentiable}}
@differentiable(wrt: x)
// expected-note @+1 {{when differentiating this function definition}}
func nonActiveInoutArg(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  return nonactive.mutatingMethod(x)
}

// CHECK-LABEL: [AD] Activity info for $s17activity_analysis17nonActiveInoutArgyAA3MutVADz_ADtF at (source=0 parameters=(1))
// CHECK: [ACTIVE] %0 = argument of bb0 : $*Mut
// CHECK: [ACTIVE] %1 = argument of bb0 : $Mut
// CHECK: [ACTIVE]   %4 = begin_access [modify] [static] %0 : $*Mut
// CHECK: [NONE]   // function_ref Mut.mutatingMethod(_:)
// CHECK: [ACTIVE]   %6 = apply %5(%1, %4) : $@convention(method) (Mut, @inout Mut) -> Mut

// expected-error @+1 {{function is not differentiable}}
@differentiable(wrt: x)
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutArgMutatingMethod(_ x: Mut) -> Mut {
  var result = x
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  result = result.mutatingMethod(result)
  return result
}

// CHECK-LABEL: [AD] Activity info for $s17activity_analysis28activeInoutArgMutatingMethodyAA3MutVADF at (source=0 parameters=(0))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Mut
// CHECK: [ACTIVE]   %2 = alloc_stack $Mut, var, name "result"
// CHECK: [ACTIVE]   %4 = begin_access [read] [static] %2 : $*Mut
// CHECK: [ACTIVE]   %5 = load [trivial] %4 : $*Mut
// CHECK: [ACTIVE]   %7 = begin_access [modify] [static] %2 : $*Mut
// CHECK: [NONE]   // function_ref Mut.mutatingMethod(_:)
// CHECK: [ACTIVE]   %9 = apply %8(%5, %7) : $@convention(method) (Mut, @inout Mut) -> Mut
// CHECK: [ACTIVE]   %11 = begin_access [modify] [static] %2 : $*Mut
// CHECK: [ACTIVE]   %14 = begin_access [read] [static] %2 : $*Mut
// CHECK: [ACTIVE]   %15 = load [trivial] %14 : $*Mut

// expected-error @+1 {{function is not differentiable}}
@differentiable(wrt: x)
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutArgMutatingMethodVar(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  var result = nonactive
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  result = result.mutatingMethod(x)
  return result
}

// CHECK_LABEL: [AD] Activity info for $s17activity_analysis31activeInoutArgMutatingMethodVaryAA3MutVADz_ADtF at (source=0 parameters=(1))
// CHECK: [USEFUL] %0 = argument of bb0 : $*Mut
// CHECK: [ACTIVE] %1 = argument of bb0 : $Mut
// CHECK: [ACTIVE]   %4 = alloc_stack $Mut, var, name "result"
// CHECK: [USEFUL]   %5 = begin_access [read] [static] %0 : $*Mut
// CHECK: [ACTIVE]   %8 = begin_access [modify] [static] %4 : $*Mut
// CHECK: [NONE]   // function_ref Mut.mutatingMethod(_:)
// CHECK: [ACTIVE]   %10 = apply %9(%1, %8) : $@convention(method) (Mut, @inout Mut) -> Mut
// CHECK: [ACTIVE]   %12 = begin_access [modify] [static] %4 : $*Mut
// CHECK: [ACTIVE]   %15 = begin_access [read] [static] %4 : $*Mut
// CHECK: [ACTIVE]   %16 = load [trivial] %15 : $*Mut

// expected-error @+1 {{function is not differentiable}}
@differentiable(wrt: x)
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutArgMutatingMethodTuple(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  var result = (nonactive, x)
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  let result2 = result.0.mutatingMethod(result.0)
  return result2
}

// CHECK-LABEL: [AD] Activity info for $s17activity_analysis33activeInoutArgMutatingMethodTupleyAA3MutVADz_ADtF at (source=0 parameters=(1))
// CHECK: [USEFUL] %0 = argument of bb0 : $*Mut
// CHECK: [ACTIVE] %1 = argument of bb0 : $Mut
// CHECK: [ACTIVE]   %4 = alloc_stack $(Mut, Mut), var, name "result"
// CHECK: [ACTIVE]   %5 = tuple_element_addr %4 : $*(Mut, Mut), 0
// CHECK: [ACTIVE]   %6 = tuple_element_addr %4 : $*(Mut, Mut), 1
// CHECK: [USEFUL]   %7 = begin_access [read] [static] %0 : $*Mut
// CHECK: [ACTIVE]   %11 = begin_access [read] [static] %4 : $*(Mut, Mut)
// CHECK: [ACTIVE]   %12 = tuple_element_addr %11 : $*(Mut, Mut), 0
// CHECK: [ACTIVE]   %13 = load [trivial] %12 : $*Mut
// CHECK: [ACTIVE]   %15 = begin_access [modify] [static] %4 : $*(Mut, Mut)
// CHECK: [ACTIVE]   %16 = tuple_element_addr %15 : $*(Mut, Mut), 0
// CHECK: [NONE]   // function_ref Mut.mutatingMethod(_:)
// CHECK: [ACTIVE]   %18 = apply %17(%13, %16) : $@convention(method) (Mut, @inout Mut) -> Mut

// Check `inout` arguments.

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutArg(_ x: Float) -> Float {
  var result = x
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  result += x
  return result
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}activeInoutArg{{.*}} at (source=0 parameters=(0))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [ACTIVE]   %2 = alloc_stack $Float, var, name "result"
// CHECK: [ACTIVE]   %5 = begin_access [modify] [static] %2 : $*Float
// CHECK: [NONE]   // function_ref static Float.+= infix(_:_:)
// CHECK: [NONE]   %7 = apply %6(%5, %0, %4) : $@convention(method) (@inout Float, Float, @thin Float.Type) -> ()
// CHECK: [ACTIVE]   %9 = begin_access [read] [static] %2 : $*Float
// CHECK: [ACTIVE]   %10 = load [trivial] %9 : $*Float

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutArgNonactiveInitialResult(_ x: Float) -> Float {
  var result: Float = 1
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  result += x
  return result
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}activeInoutArgNonactiveInitialResult{{.*}} at (source=0 parameters=(0))
// CHECK-LABEL: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK-LABEL: [ACTIVE]   %2 = alloc_stack $Float, var, name "result"
// CHECK-LABEL: [NONE]   // function_ref Float.init(_builtinIntegerLiteral:)
// CHECK-LABEL: [USEFUL]   %6 = apply %5(%3, %4) : $@convention(method) (Builtin.IntLiteral, @thin Float.Type) -> Float
// CHECK-LABEL: [USEFUL]   %8 = metatype $@thin Float.Type
// CHECK-LABEL: [ACTIVE]   %9 = begin_access [modify] [static] %2 : $*Float
// CHECK-LABEL: [NONE]   // function_ref static Float.+= infix(_:_:)
// CHECK-LABEL: [NONE]   %11 = apply %10(%9, %0, %8) : $@convention(method) (@inout Float, Float, @thin Float.Type) -> ()
// CHECK-LABEL: [ACTIVE]   %13 = begin_access [read] [static] %2 : $*Float
// CHECK-LABEL: [ACTIVE]   %14 = load [trivial] %13 : $*Float
