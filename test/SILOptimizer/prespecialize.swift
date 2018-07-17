// RUN: %target-swift-frontend  %s -Onone -Xllvm -sil-inline-generics=false -emit-sil | %FileCheck %s

// REQUIRES: optimized_stdlib

// Check that pre-specialization works at -Onone.
// This test requires the standard library to be compiled with pre-specializations!

// CHECK-LABEL: sil [noinline] @$S13prespecialize4test_4sizeySaySiGz_SitF
//
// function_ref specialized Collection<A where ...>.makeIterator() -> IndexingIterator<A>
// CHECK: function_ref @$SSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyFSnySiG_Tg5
//
// function_ref specialized IndexingIterator.next() -> A.Element?
// CHECK: function_ref @$Ss16IndexingIteratorV4next7ElementQzSgyFSnySiG_Tg5
//
// Look for generic specialization <Swift.Int> of Swift.Array.subscript.getter : (Swift.Int) -> A
// CHECK: function_ref @$SSn15uncheckedBoundsSnyxGx5lower_x5uppert_tcfCSi_Tg5
// CHECK: return
@inline(never)
public func test(_ a: inout [Int], size: Int) {
  for i in 0..<size {
    for j in 0..<size {
      a[i] = a[j]
    }
  }
}

// CHECK-LABEL: sil [noinline] @$S13prespecialize3runyyF
// Look for generic specialization <Swift.Int> of Swift.Array.init (repeating : A, count : Swift.Int) -> Swift.Array<A>
// CHECK: function_ref @$SSa9repeating5countSayxGx_SitcfCSi_Tg5
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

