// RUN: %target-swift-frontend  %s -Onone  -emit-sil | FileCheck %s

// REQUIRES: optimized_stdlib

// Check that pre-specialization works at -Onone.
// This test requires the standard library to be compiled with pre-specializations!

// CHECK-LABEL: sil [noinline] @_TF13prespecialize4testFTRGSaSi_4sizeSi_T_ 
// Look for generic specialization <Swift.Int with Swift.Int : Swift.ForwardIndex in Swift, Swift.Int with Swift.Int : Swift._SignedInteger in Swift, Swift.Int with Swift.Int : Swift._BuiltinIntegerLiteralConvertible in Swift, Swift.Int> of Swift.Range.iterator <A where A: Swift.ForwardIndex> (Swift.Range<A>)() -> Swift.RangeIterator<A>
// CHECK: function_ref @_TTSg5SiSis12ForwardIndexs_SiSis14_SignedIntegers_SiSis33_BuiltinIntegerLiteralConvertibles_Si___TFVs5Range12makeIterator
// Look for generic specialization <Swift.Int with Swift.Int : Swift.ForwardIndex in Swift, Swift.Int with Swift.Int : Swift._SignedInteger in Swift, Swift.Int with Swift.Int : Swift._BuiltinIntegerLiteralConvertible in Swift, Swift.Int> of Swift.RangeIterator.next <A where A: Swift.ForwardIndex> (inout Swift.RangeIterator<A>)() -> Swift.Optional<A>
// CHECK: function_ref @_TTSg5SiSis12ForwardIndexs_SiSis14_SignedIntegers_SiSis33_BuiltinIntegerLiteralConvertibles_Si___TFVs13RangeIterator4next
// Look for generic specialization <Swift.Int> of Swift.Array.subscript.getter : (Swift.Int) -> A
// CHECK: function_ref {{@_TTSg5Si___TFSag9subscriptFSix|@_TTSg5Si___TFSaap9subscriptFSix}}
// CHECK: return
@inline(never)
public func test(a: inout [Int], size: Int) {
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

