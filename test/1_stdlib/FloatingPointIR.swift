// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: %target-build-swift -emit-ir %s | FileCheck -check-prefix=%target-cpu %s
var globalFloat32 : Float32 = 0.0
var globalFloat64 : Float64 = 0.0
#if arch(i386) || arch(x86_64)
var globalFloat80 : Float80 = 0.0
#endif

@inline(never)
func acceptFloat32(a: Float32) {
  globalFloat32 = a
}

@inline(never)
func acceptFloat64(a: Float64) {
  globalFloat64 = a
}

#if arch(i386) || arch(x86_64)
@inline(never)
func acceptFloat80(a: Float80) {
  globalFloat80 = a
}
#endif

func testConstantFoldFloatLiterals() {
  acceptFloat32(1.0)
  acceptFloat64(1.0)
#if arch(i386) || arch(x86_64)
  acceptFloat80(1.0)
#endif
}

// i386: call void @{{.*}}_TF15FloatingPointIR13acceptFloat32FSfT_(float 1.000000e+00)
// i386: call void @{{.*}}_TF15FloatingPointIR13acceptFloat64FSdT_(double 1.000000e+00)
// i386: call void @{{.*}}_TF15FloatingPointIR13acceptFloat80FVSs7Float80T_(x86_fp80 0xK3FFF8000000000000000)

// x86_64: call void @{{.*}}_TF15FloatingPointIR13acceptFloat32FSfT_(float 1.000000e+00)
// x86_64: call void @{{.*}}_TF15FloatingPointIR13acceptFloat64FSdT_(double 1.000000e+00)
// x86_64: call void @{{.*}}_TF15FloatingPointIR13acceptFloat80FVSs7Float80T_(x86_fp80 0xK3FFF8000000000000000)

// armv7: call void @{{.*}}_TF15FloatingPointIR13acceptFloat32FSfT_(float 1.000000e+00)
// armv7: call void @{{.*}}_TF15FloatingPointIR13acceptFloat64FSdT_(double 1.000000e+00)

// arm64: call void @{{.*}}_TF15FloatingPointIR13acceptFloat32FSfT_(float 1.000000e+00)
// arm64: call void @{{.*}}_TF15FloatingPointIR13acceptFloat64FSdT_(double 1.000000e+00)

