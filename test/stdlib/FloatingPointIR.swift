// RUN: %target-build-swift -emit-ir %s | FileCheck -check-prefix=%target-cpu %s

@inline(never)
func acceptFloat32(a: Float32) {}

@inline(never)
func acceptFloat64(a: Float64) {}

#if arch(i386) || arch(x86_64)
@inline(never)
func acceptFloat80(a: Float80) {}
#endif

func testConstantFoldFloatLiterals() {
  acceptFloat32(1.0)
  acceptFloat64(1.0)
#if arch(i386) || arch(x86_64)
  acceptFloat80(1.0)
#endif
}

// i386: call void @_TF15FloatingPointIR13acceptFloat32FSfT_(float 1.000000e+00)
// i386: call void @_TF15FloatingPointIR13acceptFloat64FSdT_(double 1.000000e+00)
// i386: call void @_TF15FloatingPointIR13acceptFloat80FVSs7Float80T_(x86_fp80 0xK3FFF8000000000000000)

// x86_64: call void @_TF15FloatingPointIR13acceptFloat32FSfT_(float 1.000000e+00)
// x86_64: call void @_TF15FloatingPointIR13acceptFloat64FSdT_(double 1.000000e+00)
// x86_64: call void @_TF15FloatingPointIR13acceptFloat80FVSs7Float80T_(x86_fp80 0xK3FFF8000000000000000)

// armv7: call void @_TF15FloatingPointIR13acceptFloat32FSfT_(float 1.000000e+00)
// armv7: call void @_TF15FloatingPointIR13acceptFloat64FSdT_(double 1.000000e+00)
// armv7: call void @_TF15FloatingPointIR13acceptFloat80FVSs7Float80T_(x86_fp80 0xK3FFF8000000000000000)

// arm64: call void @_TF15FloatingPointIR13acceptFloat32FSfT_(float 1.000000e+00)
// arm64: call void @_TF15FloatingPointIR13acceptFloat64FSdT_(double 1.000000e+00)
// arm64: call void @_TF15FloatingPointIR13acceptFloat80FVSs7Float80T_(x86_fp80 0xK3FFF8000000000000000)

