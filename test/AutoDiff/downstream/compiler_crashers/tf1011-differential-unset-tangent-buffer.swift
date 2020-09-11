// RUN: not --crash %target-swift-emit-sil -enable-experimental-forward-mode-differentiation %s -verify
// REQUIRES: asserts

// TF-1011: Differential generation crash due to unset tangent buffer.

@differentiable
func arrayLiteral(_ x: Float, _ y: Float) -> [Float] {
  var result = [x * y, x * y]
  return result
}

// [AD] Original bb0: To differentiate or not to differentiate?
// [ ]   debug_value %0 : $Float, let, name "x", argno 1 // id: %2
// [ ]   debug_value %1 : $Float, let, name "y", argno 2 // id: %3
// [∂]   %4 = alloc_stack $Array<Float>, var, name "result" // users: %26, %25, %21, %22
// [ ]   %5 = integer_literal $Builtin.Word, 2           // user: %7
// [ ]   // function_ref _allocateUninitializedArray<A>(_:)
//   %6 = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer) // user: %7
// [∂]   %7 = apply %6<Float>(%5) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer) // user: %8
// [∂]   (%8, %9) = destructure_tuple %7 : $(Array<Float>, Builtin.RawPointer) // users: %21, %10
// [ ]   %10 = pointer_to_address %9 : $Builtin.RawPointer to [strict] $*Float // users: %16, %14
// [ ]   %11 = metatype $@thin Float.Type                // user: %13
// [ ]   // function_ref static Float.* infix(_:_:)
//   %12 = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float // user: %13
// [∂]   %13 = apply %12(%0, %1, %11) : $@convention(method) (Float, Float, @thin Float.Type) -> Float // user: %14
// [∂]   store %13 to [trivial] %10 : $*Float            // id: %14
// ...
// [AD] JVPEmitter visited:
// [ORIG]  store %13 to [trivial] %10 : $*Float            // id: %14
// Assertion failed: (!insertion.second && "tangent buffer should already exist"), function getTangentBuffer, file swift/lib/SILOptimizer/Mandatory/Differentiation.cpp, line 4528.

// `store %13 to [trivial] %10` is visited but `%10 = pointer_to_address %9` is
// not. `%10` does not have a set tangent buffer.
