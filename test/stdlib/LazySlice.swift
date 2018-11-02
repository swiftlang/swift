// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out3 -swift-version 3 && %target-run %t/a.out3
// REQUIRES: executable_test

import StdlibUnittest

var tests = TestSuite("LazySlice")

tests.test("CommuteLazyness") {
  let a = [1,2,3].lazy
  let b = a[...]
  var c = b.filter { $0 == 0 }
  // NOTE, this test will fail once lazy collectionness becomes a conditiona
  // conformance, and will need updating to be a LazyBidirectional thingy
  expectType(LazyFilterBidirectionalCollection<Slice<LazyRandomAccessCollection<[Int]>>>.self, &c)
}

runAllTests()
