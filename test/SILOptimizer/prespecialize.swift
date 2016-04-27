// RUN: %target-swift-frontend  %s -Onone  -emit-sil | FileCheck %s

// REQUIRES: optimized_stdlib

// Check that pre-specialization works at -Onone.
// This test requires the standard library to be compiled with pre-specializations!

// CHECK-LABEL: sil [noinline] @_TF13prespecialize4testFTRGSaSi_4sizeSi_T_ 
//
// function_ref specialized Collection<A where ...>.makeIterator() -> IndexingIterator<A>
// CHECK: function_ref @_TTSgq5GVs14CountableRangeSi_GS_Si_s10Collections_SiSis10Comparables_SiSis13SignedIntegers_SiSis33_BuiltinIntegerLiteralConvertibles_SiSis12SignedNumbers_SiSiS3_s_Si_GS_Si_GS_Si_s13IndexableBasesGS_Si_s8Sequences_SiSiS1_s_GVs16IndexingIteratorGS_Si__GS7_GS_Si__s16IteratorProtocols_Si_GS_Si__Si_GS_Si_GS_Si_S5_sGS_Si_S6_s_SiSiS1_s_GS7_GS_Si__GS7_GS_Si__S8_s_Si_GS_Si__Si_Si___TFesRxs10Collectionwx8IteratorzGVs16IndexingIteratorx_wx8_ElementzWxS0_7Element_rS_12makeIteratorfT_GS1_x_
//
// function_ref specialized IndexingIterator.next() -> A._Element?
// CHECK: function_ref @_TTSgq5GVs14CountableRangeSi_GS_Si_s13IndexableBases_SiSis10Comparables_GS_Si__Si___TFVs16IndexingIterator4nextfT_GSqwx8_Element_
//
// Look for generic specialization <Swift.Int> of Swift.Array.subscript.getter : (Swift.Int) -> A
// CHECK: function_ref {{@_TTSgq5Si___TFSag9subscriptFSix|@_TTSg5Si___TFSaap9subscriptFSix}}
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
// CHECK: function_ref @_TTSgq5Si___TFSaCfT9repeatingx5countSi_GSax_
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

