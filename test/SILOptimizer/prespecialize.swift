// RUN: %target-swift-frontend  %s -Onone  -emit-sil | FileCheck %s

// REQUIRES: optimized_stdlib

// Check that pre-specialization works at -Onone.
// This test requires the standard library to be compiled with pre-specializations!

// CHECK-LABEL: sil [noinline] @_TF13prespecialize4testFTRGSaSi_4sizeSi_T_ 
//
// function_ref specialized CountableRange.makeIterator() -> CountableRangeIterator<A>
// CHECK: function_ref @_TTSg5SiSis10Strideables_SiSis12SignedNumbers_SiSis33_BuiltinIntegerLiteralConvertibles___TFVs17CountableRange12makeIteratorfT_GVs25CountableRangeIteratorx_
//
// function_ref specialized CountableRangeIterator.next() -> A?
// CHECK: function_ref @_TTSg5SiSis10Strideables_SiSis12SignedNumbers_SiSis33_BuiltinIntegerLiteralConvertibles___TFVs25CountableRangeIterator4nextfT_GSqx_
//
// Look for generic specialization <Swift.Int> of Swift.Array.subscript.getter : (Swift.Int) -> A
// CHECK: function_ref {{@_TTSg5Si___TFSag9subscriptFSix|@_TTSg5Si___TFSaap9subscriptFSix}}
// CHECK: return
@inline(never)
public func test(_ a: inout [Int], size: Int) {
  for i in 0..<size {
    for j in 0..<size {
      a[i] = a[j]
    }
  }
}

// CHECK-LABEL: sil [noinline] @_TF13prespecialize3runFT_T_
// Look for generic specialization <Swift.Int> of Swift.Array.init (repeating : A, count : Swift.Int) -> Swift.Array<A>
// CHECK: function_ref @_TTSg5Si___TFSaCfT9repeatingx5countSi_GSax_
// CHECK: return
@inline(never)
public func run() {
  let size = 10000
  var p = [Int](repeating: 0, count: size)
  for i in 0..<size {
    p[i] = i
  }
  test(&p, size: size)
}

run()

