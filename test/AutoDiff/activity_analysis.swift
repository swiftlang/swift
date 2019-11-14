// RUN: %target-swift-emit-sil -Xllvm -debug-only=differentiation 2>&1 %s | %FileCheck %s

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
// CHECK: [VARIED]   %9 = pointer_to_address %8 : $Builtin.RawPointer to [strict] $*Float
// CHECK: [VARIED]   %11 = integer_literal $Builtin.Word, 1
// CHECK: [VARIED]   %12 = index_addr %9 : $*Float, %11 : $Builtin.Word

@differentiable(where T: Differentiable)
func testArrayUninitializedIntrinsicGeneric<T>(_ x: T, _ y: T) -> [T] {
  return [x, y]
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}testArrayUninitializedIntrinsicGeneric{{.*}} at (source=0 parameters=(0 1))
// CHECK: [VARIED] %0 = argument of bb0 : $*T
// CHECK: [VARIED] %1 = argument of bb0 : $*T
// CHECK: [USEFUL]   %4 = integer_literal $Builtin.Word, 2
// CHECK: [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [ACTIVE]   %6 = apply %5<T>(%4) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [ACTIVE] (**%7**, %8) = destructure_tuple %6 : $(Array<T>, Builtin.RawPointer)
// CHECK: [VARIED] (%7, **%8**) = destructure_tuple %6 : $(Array<T>, Builtin.RawPointer)
// CHECK: [VARIED]   %9 = pointer_to_address %8 : $Builtin.RawPointer to [strict] $*T
// CHECK: [VARIED]   %11 = integer_literal $Builtin.Word, 1
// CHECK: [VARIED]   %12 = index_addr %9 : $*T, %11 : $Builtin.Word

// TF-952: Test array literal initialized from an address (e.g. `var`).
@differentiable
func testArrayUninitializedIntrinsicAddress(_ x: Float, _ y: Float) -> [Float] {
  var result = x
  result = result * y
  return [result, result]
}
// CHECK-LABEL: [AD] Activity info for ${{.*}}testArrayUninitializedIntrinsicAddress{{.*}} at (source=0 parameters=(0 1))
// CHECK: [VARIED] %0 = argument of bb0 : $Float
// CHECK: [VARIED] %1 = argument of bb0 : $Float
// CHECK: [VARIED]   %4 = alloc_stack $Float, var, name "result"
// CHECK: [VARIED]   %7 = begin_access [read] [static] %4 : $*Float
// CHECK: [VARIED]   %8 = load [trivial] %7 : $*Float
// CHECK: [NONE]   // function_ref static Float.* infix(_:_:)
// CHECK: [VARIED]   %11 = apply %10(%8, %1, %6) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK: [VARIED]   %12 = begin_access [modify] [static] %4 : $*Float
// CHECK: [USEFUL]   %15 = integer_literal $Builtin.Word, 2
// CHECK: [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [ACTIVE]   %17 = apply %16<Float>(%15) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [ACTIVE] (**%18**, %19) = destructure_tuple %17 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [VARIED] (%18, **%19**) = destructure_tuple %17 : $(Array<Float>, Builtin.RawPointer)
// CHECK: [VARIED]   %20 = pointer_to_address %19 : $Builtin.RawPointer to [strict] $*Float
// CHECK: [VARIED]   %21 = begin_access [read] [static] %4 : $*Float
// CHECK: [VARIED]   %24 = integer_literal $Builtin.Word, 1
// CHECK: [VARIED]   %25 = index_addr %20 : $*Float, %24 : $Builtin.Word
// CHECK: [VARIED]   %26 = begin_access [read] [static] %4 : $*Float

// TF-952: Test array literal initialized with function call results.
@differentiable
func testArrayUninitializedIntrinsicFunctionResult(_ x: Float, _ y: Float) -> [Float] {
  return [x * y, x * y]
}
// CHECK-LABEL: [AD] Activity info for ${{.*}}testArrayUninitializedIntrinsicFunctionResult{{.*}} at (source=0 parameters=(0 1))
// [ACTIVE] %0 = argument of bb0 : $Float
// [ACTIVE] %1 = argument of bb0 : $Float
// [USEFUL]   %4 = integer_literal $Builtin.Word, 2
// [NONE]   // function_ref _allocateUninitializedArray<A>(_:)
// [ACTIVE]   %6 = apply %5<Float>(%4) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// [ACTIVE] (**%7**, %8) = destructure_tuple %6 : $(Array<Float>, Builtin.RawPointer)
// [VARIED] (%7, **%8**) = destructure_tuple %6 : $(Array<Float>, Builtin.RawPointer)
// [VARIED]   %9 = pointer_to_address %8 : $Builtin.RawPointer to [strict] $*Float
// [NONE]   // function_ref static Float.* infix(_:_:)
// [ACTIVE]   %12 = apply %11(%0, %1, %10) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// [VARIED]   %14 = integer_literal $Builtin.Word, 1
// [VARIED]   %15 = index_addr %9 : $*Float, %14 : $Builtin.Word
// [USEFUL]   %16 = metatype $@thin Float.Type
// [NONE]   // function_ref static Float.* infix(_:_:)
// [ACTIVE]   %18 = apply %17(%0, %1, %16) : $@convention(method) (Float, Float, @thin Float.Type) -> Float

// TF-781: check active local address + nested conditionals.

@differentiable(wrt: x)
func TF_781(_ x: Float, _ y: Float) -> Float {
  var result = y
  if true {
    if true {
      result = result * x
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
