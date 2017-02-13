// RUN: %target-swift-frontend  %s -Onone  -emit-sil | %FileCheck %s

// REQUIRES: optimized_stdlib

// FIXME: https://bugs.swift.org/browse/SR-2808
// XFAIL: resilient_stdlib

// Check that pre-specialization works at -Onone.
// This test requires the standard library to be compiled with pre-specializations!

// CHECK-LABEL: sil [noinline] @_TF13prespecialize4testFTRGSaSi_4sizeSi_T_ 
//
// function_ref specialized Collection<A where ...>.makeIterator() -> IndexingIterator<A>
// CHECK: function_ref @_TTSgq5SiSis10ComparablesSis11_Strideables___TFVs14CountableRangeCfT15uncheckedBoundsT5lowerx5upperx__GS_x_
//
// function_ref specialized IndexingIterator.next() -> A._Element?
// CHECK: function_ref @_TTSgq5GVs14CountableRangeSi_GS_Si_s14_IndexableBases___TFVs16IndexingIterator4nextfT_GSqwx8_Element_
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

