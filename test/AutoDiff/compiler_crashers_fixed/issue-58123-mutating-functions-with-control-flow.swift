// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -debug-info-format=dwarf -emit-module -module-name M -emit-module-path %t/M.swiftmodule -swift-version 5 -c %s

// https://github.com/apple/swift/issues/58123
// Mutating functions with control flow can cause assertion failure for
// conflicting debug variable type

import _Differentiation

// Declare `Tensor`

class TensorHandle {} // Crash requires `class` and not `struct`

struct Tensor<Scalar> {
  let handle: TensorHandle
}

extension Tensor: Differentiable {
  typealias TangentVector = Tensor
}

extension Tensor: AdditiveArithmetic  {
  static func == (lhs: Tensor, rhs: Tensor) -> Bool { fatalError() }
  static func != (lhs: Tensor, rhs: Tensor) -> Bool { fatalError() }
  
  static var zero: Tensor { fatalError() }

  @differentiable(reverse)
  static func + (lhs: Tensor, rhs: Tensor) -> Tensor { fatalError() }
  static func - (lhs: Tensor, rhs: Tensor) -> Tensor { fatalError() }
}

// Make `+=` differentiable

extension Tensor {
  @derivative(of: +)
  static func _vjpAdd(lhs: Tensor, rhs: Tensor) -> (
    value: Tensor, pullback: (Tensor) -> (Tensor, Tensor)
  ) {
    fatalError()
  }
  
  static func += (lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs + rhs
  }
}

// Declare `BatchNorm`

protocol Layer: Differentiable {
  associatedtype Input
  associatedtype Output: Differentiable

  @differentiable(reverse)
  func callAsFunction(_ input: Input) -> Output
}

struct BatchNorm<Scalar>: Layer { // Crash requires conformance to `Layer`
  @noDerivative let momentum: Scalar // Crash requires `@noDerivative`
  var offset: Tensor<Scalar>

  @differentiable(reverse)
  func callAsFunction(_ input: Tensor<Scalar>) -> Tensor<Scalar> {
    var offset = self.offset
    // TODO: cannot use literal `true` because it crashes
    if 1 == 1 { // Crash requires `if true`
      offset += offset // Using `offset = offset + offset` stops the crash
    }
    return offset
  }
}

// Original crash:

/*
SIL verification failed: conflicting debug variable type!: DebugVars[argNum].second == DebugVarTy
Verifying instruction:
     %5 = apply %4<τ_0_0>(%3) : $@convention(method) <τ_0_0> (@thin BatchNorm<τ_0_0>.TangentVector.Type) -> @owned BatchNorm<τ_0_0>.TangentVector // users: %39, %69, %47, %13, %40, %20
->   debug_value %5 : $BatchNorm<τ_0_0>.TangentVector, let, name "self", argno 2, implicit, expr op_deref // id: %20
In function:
// pullback of BatchNorm.callAsFunction(_:)
sil private @$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGFlTJpUSpSr : $@convention(thin) <τ_0_0> (@guaranteed _Tensor<τ_0_0>, @owned _AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb3__PB__src_0_wrt_1_l<τ_0_0>) -> @out BatchNorm<τ_0_0>.TangentVector {
// %0                                             // user: %87
// %1                                             // user: %17
// %2                                             // users: %50, %16
bb0(%0 : $*BatchNorm<τ_0_0>.TangentVector, %1 : $_Tensor<τ_0_0>, %2 : $_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb3__PB__src_0_wrt_1_l<τ_0_0>):
  %3 = metatype $@thin BatchNorm<τ_0_0>.TangentVector.Type // user: %5
  // function_ref static BatchNorm.TangentVector.zero.getter
  %4 = function_ref @$s10TensorFlow9BatchNormV13TangentVectorV4zeroAEyx_GvgZ : $@convention(method) <τ_0_0> (@thin BatchNorm<τ_0_0>.TangentVector.Type) -> @owned BatchNorm<τ_0_0>.TangentVector // user: %5
  %5 = apply %4<τ_0_0>(%3) : $@convention(method) <τ_0_0> (@thin BatchNorm<τ_0_0>.TangentVector.Type) -> @owned BatchNorm<τ_0_0>.TangentVector // users: %39, %69, %47, %13, %40, %20
  %6 = alloc_stack $_Tensor<τ_0_0>, var, name "offset" // users: %41, %19, %33, %27, %21, %10, %91
  %7 = metatype $@thin _Tensor<τ_0_0>.Type        // user: %9
  // function_ref static _Tensor.zero.getter
  %8 = function_ref @$s10TensorFlow01_A0V4zeroACyxGvgZ : $@convention(method) <τ_0_0> (@thin _Tensor<τ_0_0>.Type) -> @owned _Tensor<τ_0_0> // user: %9
  %9 = apply %8<τ_0_0>(%7) : $@convention(method) <τ_0_0> (@thin _Tensor<τ_0_0>.Type) -> @owned _Tensor<τ_0_0> // users: %26, %43, %68, %74, %53, %62, %79, %81, %82, %11, %10
  store %9 to %6 : $*_Tensor<τ_0_0>               // id: %10
  debug_value %9 : $_Tensor<τ_0_0>, var, name "offset" // id: %11
  %12 = alloc_stack $_Tensor<τ_0_0>, let, (name "self", loc "/Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Layers/Normalization.swift":70:8, scope 0), argno 2, implicit, type $*BatchNorm<τ_0_0>.TangentVector, expr op_fragment:#BatchNorm.TangentVector.offset // users: %84, %57, %70, %85, %72, %59, %14, %90
  %13 = struct_extract %5 : $BatchNorm<τ_0_0>.TangentVector, #BatchNorm.TangentVector.offset // users: %72, %59, %14
  store %13 to %12 : $*_Tensor<τ_0_0>             // id: %14
  %15 = alloc_stack $_Tensor<τ_0_0>, var, name "offset" // users: %56, %22, %35, %29, %23, %49, %89
  %16 = struct_extract %2 : $_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb3__PB__src_0_wrt_1_l<τ_0_0>, #_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb3__PB__src_0_wrt_1_l.predecessor // user: %44
  debug_value %1 : $_Tensor<τ_0_0>                // id: %17
  // function_ref specialized static _Tensor.+= infix(_:_:)
  %18 = function_ref @$s10TensorFlow01_A0V2peoiyyACyxGz_AEtFZTf4ndd_n : $@convention(thin) <τ_0_0> (@inout _Tensor<τ_0_0>) -> () // users: %84, %56, %19
  %19 = apply %18<τ_0_0>(%6) : $@convention(thin) <τ_0_0> (@inout _Tensor<τ_0_0>) -> ()
  debug_value %5 : $BatchNorm<τ_0_0>.TangentVector, let, name "self", argno 2, implicit, expr op_deref // id: %20
  %21 = load %6 : $*_Tensor<τ_0_0>                // users: %24, %23
  %22 = struct_element_addr %15 : $*_Tensor<τ_0_0>, #_Tensor.handle // users: %61, %66, %34, %28
  store %21 to %15 : $*_Tensor<τ_0_0>             // id: %23
  %24 = struct_extract %21 : $_Tensor<τ_0_0>, #_Tensor.handle // user: %25
  strong_retain %24 : $_TensorHandle              // id: %25
  release_value %9 : $_Tensor<τ_0_0>              // id: %26
  %27 = load %6 : $*_Tensor<τ_0_0>                // users: %30, %29
  %28 = load %22 : $*_TensorHandle                // user: %32
  store %27 to %15 : $*_Tensor<τ_0_0>             // id: %29
  %30 = struct_extract %27 : $_Tensor<τ_0_0>, #_Tensor.handle // user: %31
  strong_retain %30 : $_TensorHandle              // id: %31
  strong_release %28 : $_TensorHandle             // id: %32
  %33 = load %6 : $*_Tensor<τ_0_0>                // users: %36, %35
  %34 = load %22 : $*_TensorHandle                // user: %38
  store %33 to %15 : $*_Tensor<τ_0_0>             // id: %35
  %36 = struct_extract %33 : $_Tensor<τ_0_0>, #_Tensor.handle // user: %37
  strong_retain %36 : $_TensorHandle              // id: %37
  strong_release %34 : $_TensorHandle             // id: %38
  release_value %5 : $BatchNorm<τ_0_0>.TangentVector // id: %39
  debug_value %5 : $BatchNorm<τ_0_0>.TangentVector, let, name "self", argno 2, implicit, expr op_deref // id: %40
  %41 = struct_element_addr %6 : $*_Tensor<τ_0_0>, #_Tensor.handle // user: %42
  %42 = load %41 : $*_TensorHandle                // users: %46, %75
  release_value %9 : $_Tensor<τ_0_0>              // id: %43
  switch_enum %16 : $_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb3__Pred__src_0_wrt_1_l<τ_0_0>, case #_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb3__Pred__src_0_wrt_1_l.bb1!enumelt: bb1, case #_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb3__Pred__src_0_wrt_1_l.bb2!enumelt: bb2 // id: %44

// %45                                            // user: %48
bb1(%45 : $_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb1__PB__src_0_wrt_1_l<τ_0_0>): // Preds: bb0
  strong_release %42 : $_TensorHandle             // id: %46
  release_value %5 : $BatchNorm<τ_0_0>.TangentVector // id: %47
  %48 = struct_extract %45 : $_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb1__PB__src_0_wrt_1_l<τ_0_0>, #_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb1__PB__src_0_wrt_1_l.pullback_0 // user: %49
  %49 = apply %48(%15) : $@callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 == τ_0_1, τ_0_2 == τ_0_3> (@inout _Tensor<τ_0_0>) -> @owned _Tensor<τ_0_2> for <τ_0_0, τ_0_0, τ_0_0, τ_0_0> // user: %54
  release_value %2 : $_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb3__PB__src_0_wrt_1_l<τ_0_0> // id: %50
  // function_ref specialized static _Tensor.+ infix(_:_:)
  %51 = function_ref @$s10TensorFlow01_A0V1poiyACyxGAE_AEtFZTf4ddd_n : $@convention(thin) <τ_0_0> () -> @owned _Tensor<τ_0_0> // user: %52
  %52 = apply %51<τ_0_0>() : $@convention(thin) <τ_0_0> () -> @owned _Tensor<τ_0_0> // users: %63, %55
  release_value %9 : $_Tensor<τ_0_0>              // id: %53
  release_value %49 : $_Tensor<τ_0_0>             // id: %54
  debug_value %52 : $_Tensor<τ_0_0>               // id: %55
  %56 = apply %18<τ_0_0>(%15) : $@convention(thin) <τ_0_0> (@inout _Tensor<τ_0_0>) -> ()
  %57 = struct_element_addr %12 : $*_Tensor<τ_0_0>, #_Tensor.handle // user: %58
  %58 = load %57 : $*_TensorHandle                // user: %60
  store %13 to %12 : $*_Tensor<τ_0_0>             // id: %59
  strong_release %58 : $_TensorHandle             // id: %60
  %61 = load %22 : $*_TensorHandle                // user: %64
  release_value %9 : $_Tensor<τ_0_0>              // id: %62
  release_value %52 : $_Tensor<τ_0_0>             // id: %63
  br bb3(%61 : $_TensorHandle)                    // id: %64

bb2(%65 : $_AD__$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGF_bb2__PB__src_0_wrt_1_l<τ_0_0>): // Preds: bb0
  %66 = load %22 : $*_TensorHandle                // user: %67
  strong_release %66 : $_TensorHandle             // id: %67
  release_value %9 : $_Tensor<τ_0_0>              // id: %68
  release_value %5 : $BatchNorm<τ_0_0>.TangentVector // id: %69
  %70 = struct_element_addr %12 : $*_Tensor<τ_0_0>, #_Tensor.handle // user: %71
  %71 = load %70 : $*_TensorHandle                // user: %73
  store %13 to %12 : $*_Tensor<τ_0_0>             // id: %72
  strong_release %71 : $_TensorHandle             // id: %73
  release_value %9 : $_Tensor<τ_0_0>              // id: %74
  br bb3(%42 : $_TensorHandle)                    // id: %75

// %76                                            // user: %80
bb3(%76 : $_TensorHandle):                        // Preds: bb1 bb2
  // function_ref specialized static _Tensor.+ infix(_:_:)
  %77 = function_ref @$s10TensorFlow01_A0V1poiyACyxGAE_AEtFZTf4ddd_n : $@convention(thin) <τ_0_0> () -> @owned _Tensor<τ_0_0> // user: %78
  %78 = apply %77<τ_0_0>() : $@convention(thin) <τ_0_0> () -> @owned _Tensor<τ_0_0> // users: %88, %83
  release_value %9 : $_Tensor<τ_0_0>              // id: %79
  strong_release %76 : $_TensorHandle             // id: %80
  release_value %9 : $_Tensor<τ_0_0>              // id: %81
  debug_value %9 : $_Tensor<τ_0_0>, var, name "offset" // id: %82
  debug_value %78 : $_Tensor<τ_0_0>               // id: %83
  %84 = apply %18<τ_0_0>(%12) : $@convention(thin) <τ_0_0> (@inout _Tensor<τ_0_0>) -> ()
  %85 = load %12 : $*_Tensor<τ_0_0>               // user: %86
  %86 = struct $BatchNorm<τ_0_0>.TangentVector (%85 : $_Tensor<τ_0_0>) // user: %87
  store %86 to %0 : $*BatchNorm<τ_0_0>.TangentVector // id: %87
  release_value %78 : $_Tensor<τ_0_0>             // id: %88
  dealloc_stack %15 : $*_Tensor<τ_0_0>            // id: %89
  dealloc_stack %12 : $*_Tensor<τ_0_0>            // id: %90
  dealloc_stack %6 : $*_Tensor<τ_0_0>             // id: %91
  %92 = tuple ()                                  // user: %93
  return %92 : $()                                // id: %93
} // end sil function '$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGFlTJpUSpSr'

Please submit a bug report (https://swift.org/contributing/#reporting-bugs) and include the project and the crash backtrace.
Stack dump:
0.  Program arguments: /Library/Developer/Toolchains/swift-DEVELOPMENT-SNAPSHOT-2022-02-03-a.xctoolchain/usr/bin/swift-frontend -frontend -c /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Bindings/EagerExecution.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Bindings/RawOpsAugmented.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Bindings/RawOpsDispatching.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Bindings/RawOpsGenerated.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Bindings/TFTensorOperation.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/BroadcastingPullback.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/CopyableToDevice.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/DataTypes.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/DifferentialOperators.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/ElementaryFunctions.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/EuclideanDifferentiable.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/Execution.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/KeyPathIterable.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/LazyTensorContext.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/LazyTensorOperation.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/LazyTensorShapeInference.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/LazyTensorTFFunctionBuilder.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/LazyTensorTrace.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/LazyTensorTraceCache.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/MixedPrecision.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/PointwiseMultiplicative.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/Runtime.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/ShapedArray.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/StringTensor.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/Tensor.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/TensorGroup.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/TensorHandle.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/TensorShape.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/Threading.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/Utilities.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Core/VectorProtocol.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Initializers.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Layers/Normalization.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Operators/Basic.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Operators/Math.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/StdlibExtensions.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/X10/APIs/CrossReplicaSum.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/X10/APIs/DeviceScope.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/X10/APIs/RawOpsManual.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/X10/Device.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/X10/RawOpsXLAGenerated.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/X10/XLAScalarType.swift /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/X10/XLATensor.swift -supplementary-output-file-map /var/folders/qn/86czb43d3pv03bfnxvb3x66h0000gn/T/TemporaryDirectory.wBRIea/supplementaryOutputs-1 -target arm64-apple-macosx10.13 -Xllvm -aarch64-use-tbi -enable-objc-interop -sdk /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.1.sdk -I /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release -I /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/usr/lib -F /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/Library/Frameworks -color-diagnostics -g -debug-info-format=dwarf -module-cache-path /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/ModuleCache -swift-version 5 -O -D SWIFT_PACKAGE -D DEFAULT_BACKEND_EAGER -D TENSORFLOW_USE_STANDARD_TOOLCHAIN -new-driver-path /Library/Developer/Toolchains/swift-DEVELOPMENT-SNAPSHOT-2022-02-03-a.xctoolchain/usr/bin/swift-driver -resource-dir /Library/Developer/Toolchains/swift-DEVELOPMENT-SNAPSHOT-2022-02-03-a.xctoolchain/usr/lib/swift -Xcc -fmodule-map-file=/Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/checkouts/swift-numerics/Sources/_NumericsShims/include/module.modulemap -Xcc -I -Xcc /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/checkouts/swift-numerics/Sources/_NumericsShims/include -Xcc -fmodule-map-file=/Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/CX10Modules/include/module.modulemap -Xcc -I -Xcc /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/CX10Modules/include -Xcc -fmodule-map-file=/Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/CTensorFlow/include/module.modulemap -Xcc -I -Xcc /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/CTensorFlow/include -Xcc -I/Users/philipturner/Documents/building-tensorflow/Library/tensorflow-2.8.0/usr/include -module-name TensorFlow -target-sdk-version 12.1 -parse-as-library -num-threads 10 -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Bindings/EagerExecution.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Bindings/RawOpsAugmented.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Bindings/RawOpsDispatching.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Bindings/RawOpsGenerated.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Bindings/TFTensorOperation.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/BroadcastingPullback.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/CopyableToDevice.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/DataTypes.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/DifferentialOperators.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/ElementaryFunctions.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/EuclideanDifferentiable.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/Execution.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/KeyPathIterable.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/LazyTensorContext.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/LazyTensorOperation.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/LazyTensorShapeInference.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/LazyTensorTFFunctionBuilder.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/LazyTensorTrace.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/LazyTensorTraceCache.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/MixedPrecision.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/PointwiseMultiplicative.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/Runtime.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/ShapedArray.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/StringTensor.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/Tensor.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/TensorGroup.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/TensorHandle.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/TensorShape.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/Threading.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/Utilities.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Core/VectorProtocol.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Initializers.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Layers/Normalization.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Operators/Basic.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/Operators/Math.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/StdlibExtensions.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/X10/APIs/CrossReplicaSum.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/X10/APIs/DeviceScope.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/X10/APIs/RawOpsManual.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/X10/Device.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/X10/RawOpsXLAGenerated.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/X10/XLAScalarType.swift.o -o /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/.build/arm64-apple-macosx/release/TensorFlow.build/X10/XLATensor.swift.o
1.  Apple Swift version 5.7-dev (LLVM cd2992b90c95d01, Swift ab85807f0646caa)
2.  Compiling with the current language version
3.  While verifying SIL function "@$s10TensorFlow9BatchNormV14callAsFunctionyAA01_A0VyxGAGFlTJpUSpSr".
 for 'callAsFunction(_:)' (at /Users/philipturner/Documents/building-tensorflow/swift-for-tensorflow/Sources/TensorFlow/Layers/Normalization.swift:70:3)
Stack dump without symbol names (ensure you have llvm-symbolizer in your PATH or set the environment var `LLVM_SYMBOLIZER_PATH` to point to it):
0  swift-frontend           0x0000000108961c54 llvm::sys::PrintStackTrace(llvm::raw_ostream&, int) + 56
1  swift-frontend           0x0000000108960eb4 llvm::sys::RunSignalHandlers() + 128
2  swift-frontend           0x00000001089622b8 SignalHandler(int) + 304
3  libsystem_platform.dylib 0x00000001bb5304e4 _sigtramp + 56
4  libsystem_pthread.dylib  0x00000001bb518eb0 pthread_kill + 288
5  libsystem_c.dylib        0x00000001bb456314 abort + 164
6  swift-frontend           0x00000001046aec58 (anonymous namespace)::SILVerifier::_require(bool, llvm::Twine const&, std::__1::function<void ()> const&) + 1432
7  swift-frontend           0x00000001046c8c6c (anonymous namespace)::SILVerifier::visitSILInstruction(swift::SILInstruction*) + 5104
8  swift-frontend           0x00000001046b25e8 (anonymous namespace)::SILVerifier::visitSILBasicBlock(swift::SILBasicBlock*) + 1204
9  swift-frontend           0x00000001046b1090 (anonymous namespace)::SILVerifier::visitSILFunction(swift::SILFunction*) + 7636
10 swift-frontend           0x00000001046ae150 swift::SILModule::verify() const + 216
11 swift-frontend           0x0000000104570a14 swift::CompilerInstance::performSILProcessing(swift::SILModule*) + 636
12 swift-frontend           0x0000000104517814 performCompileStepsPostSILGen(swift::CompilerInstance&, std::__1::unique_ptr<swift::SILModule, std::__1::default_delete<swift::SILModule> >, llvm::PointerUnion<swift::ModuleDecl*, swift::SourceFile*>, swift::PrimarySpecificPaths const&, int&, swift::FrontendObserver*) + 716
13 swift-frontend           0x00000001045171d8 swift::performCompileStepsPostSema(swift::CompilerInstance&, int&, swift::FrontendObserver*) + 1028
14 swift-frontend           0x0000000104518c74 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) + 2940
15 swift-frontend           0x0000000104454900 swift::mainEntry(int, char const**) + 500
16 dyld                     0x0000000110f490f4 start + 520
*/
