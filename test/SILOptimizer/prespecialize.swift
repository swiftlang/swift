// RUN: %target-swift-frontend  %s -Onone -Xllvm -sil-inline-generics=false -emit-sil | %FileCheck %s

// REQUIRES: optimized_stdlib

// Check that pre-specialization works at -Onone.
// This test requires the standard library to be compiled with pre-specializations!

// CHECK-LABEL: sil [noinline] @_T013prespecialize4testySaySiGz_Si4sizetF 
//
// function_ref specialized Collection<A where ...>.makeIterator() -> IndexingIterator<A>
// CHECK: function_ref @_T0s10CollectionPss16IndexingIteratorVyxG0C0RtzrlE04makeC0AEyFs14CountableRangeVySiG_Tg5
//
// function_ref specialized IndexingIterator.next() -> A.Element?
// CHECK: function_ref @_T0s16IndexingIteratorV4next7ElementQzSgyFs14CountableRangeVySiG_Tg5
//
// Look for generic specialization <Swift.Int> of Swift.Array.subscript.getter : (Swift.Int) -> A
// CHECK: function_ref {{@_T0SaxSicigSi_Tg5|@_TTSg5Si___TFSaap9subscriptFSix}}
// CHECK: return
@inline(never)
public func test(_ a: inout [Int], size: Int) {
  for i in 0..<size {
    for j in 0..<size {
      a[i] = a[j]
    }
  }
}

// CHECK-LABEL: sil [noinline] @_T013prespecialize3runyyF
// Look for generic specialization <Swift.Int> of Swift.Array.init (repeating : A, count : Swift.Int) -> Swift.Array<A>
// CHECK: function_ref @_T0S2ayxGx9repeating_Si5counttcfCSi_Tg5
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

