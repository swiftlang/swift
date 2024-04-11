// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test

// Test that noncopyable types have strict lexical lifetimes. The compiler implements this by running a lifetime
// maximization algorithm prior to optimization, and preventing optimizations from shortening the lifetime of
// noncopyable values.

struct S : ~Copyable {
  deinit {
    print("S.deinit")
  }
}

struct SS : ~Copyable {
  var s1 = S()
  var s2 = S()
  deinit {
    print("SS.deinit")
  }
}

func consumeS(_ s: consuming S) {}
func consumeS(_ ss: consuming SS) {}
func borrowS(_ s: borrowing S) {}
func borrowS(_ ss: borrowing SS) {}

// CHECK: testLexical
// CHECK: S.deinit
func testLet() {
  let s = S()
  borrowS(s)
  print("testLexical")
}

// CHECK: testConsumingParam
// CHECK: S.deinit
func testConsumingParam(_ s: consuming S) {
  borrowS(s)
  print("testConsumingParam")
}

// CHECK: testConditionalBorrow
// CHECK: S.deinit
func testConditionalBorrow(_ z: Bool) {
  let s = S()
  if z {
    borrowS(s)
  }
  print("testConditionalBorrow")
}

// CHECK: no consume
// CHECK: S.deinit
// CHECK: testConditionalConsume
func testConditionalConsume(_ z: Bool) {
  let s = S()
  if z {
    print("no consume")
  } else {
    consumeS(s)
  }
  print("testConditionalConsume")
}

// CHECK: S.deinit
// CHECK: testReassign1
// CHECK: SS.deinit
// CHECK: S.deinit
// CHECK: S.deinit
// CHECK: testReassign2
// CHECK: SS.deinit
// CHECK: S.deinit
// CHECK: S.deinit
func testReassign() {
  var ss = SS(s1: S(), s2: S())
  ss.s1 = S()
  print("testReassign1")
  ss = SS(s1: S(), s2: S())
  print("testReassign2")
}

testLet()
testConsumingParam(S())
testConditionalBorrow(true)
testConditionalConsume(true)
testReassign()
