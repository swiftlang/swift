// RUN: %target-build-swift -g %s
// REQUIRES: asserts

// TF-1232: IRGenDebugInfo crash due to lack of proper mangling for
// AutoDiff-generated declarations: linear map structs and branching trace
// enums.

import _Differentiation

extension Float {
  @_silgen_name("add")
  static func add(_ x: Float, _ y: Float) -> Float {
    return x + y
  }

  @derivative(of: add)
  static func addVJP(_ x: Float, _ y: Float) -> (
    value: Float, pullback: (Float) -> (Float, Float)
  ) {
    return (add(x, y), { v in (v, v) })
  }
}

@differentiable
func foo(_ x: Float) -> Float {
  let y = Float.add(x, x)
  return y
}

// Failed to reconstruct type for $s4main42_AD__$s4main3fooyS2fF_bb0__PB__src_0_wrt_0VmD
// Original type:
// (metatype_type
//   (struct_type decl=main.(file)._AD__$s4main3fooyS2fF_bb0__PB__src_0_wrt_0))
// Assertion failed: (isa<X>(Val) && "cast<Ty>() argument of incompatible type!"), function cast, file /Users/danielzheng/swift-merge/llvm-project/llvm/include/llvm/Support/Casting.h, line 264.
