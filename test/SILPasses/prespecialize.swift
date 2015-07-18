// RUN: %target-swift-frontend  %s -Onone -Xllvm -use-prespecialized -emit-sil | FileCheck %s

// REQUIRES: optimized_stdlib

// Check that pre-specialization works at -Onone.
// This test requires the standard library to be compiled with pre-specializations!

// CHECK-LABEL: sil [noinline] @_TF13prespecialize4testFTRGSaSi_4sizeSi_T_ 
// Look for generic specialization <Swift.Int with Swift.Int : Swift.ForwardIndexType in Swift, Swift.Int with Swift.Int : Swift._SignedIntegerType in Swift, Swift.Int with Swift.Int : Swift._BuiltinIntegerLiteralConvertible in Swift, Swift.Int> of Swift.Range.generate <A where A: Swift.ForwardIndexType> (Swift.Range<A>)() -> Swift.RangeGenerator<A>
// CHECK: function_ref @_TTSg5SiSiSs16ForwardIndexTypeSs_SiSiSs18_SignedIntegerTypeSs_SiSiSs33_BuiltinIntegerLiteralConvertibleSs_Si___TFVSs5Range8generateuRq_Ss16ForwardIndexType_fGS_q__FT_GVSs14RangeGeneratorq__
// Look for generic specialization <Swift.Int with Swift.Int : Swift.ForwardIndexType in Swift, Swift.Int with Swift.Int : Swift._SignedIntegerType in Swift, Swift.Int with Swift.Int : Swift._BuiltinIntegerLiteralConvertible in Swift, Swift.Int> of Swift.RangeGenerator.next <A where A: Swift.ForwardIndexType> (inout Swift.RangeGenerator<A>)() -> Swift.Optional<A>
// CHECK: function_ref @_TTSg5SiSiSs16ForwardIndexTypeSs_SiSiSs18_SignedIntegerTypeSs_SiSiSs33_BuiltinIntegerLiteralConvertibleSs_Si___TFVSs14RangeGenerator4nextuRq_Ss16ForwardIndexType_fRGS_q__FT_GSqq__
// Look for generic specialization <Swift.Int> of Swift.Array.subscript.getter : (Swift.Int) -> A
// CHECK: function_ref {{@_TTSg5Si___TFSag9subscriptFSiq_|@_TTSg5Si___TFSaap9subscriptFSiq_}}
// CHECK: return
@inline(never)
public func test(inout a: [Int], size: Int) {
  for i in 0..<size {
    for j in 0..<size {
      a[i] = a[j]
    }
  }
}

// CHECK-LABEL: sil [noinline] @_TF13prespecialize3runFT_T_
// Look for generic specialization <Swift.Int> of Swift.Array.init <A> (Swift.Array<A>.Type)(count : Swift.Int, repeatedValue : A) -> Swift.Array<A>
// CHECK: function_ref @_TTSg5Si___TFSaCurfMGSaq__FT5countSi13repeatedValueq__GSaq__
// CHECK: return
@inline(never)
public func run() {
  let size = 10000
  var p = [Int](count: size, repeatedValue: 0)
  for i in 0..<size {
    p[i] = i
  }
  test(&p, size: size)
}

run()

