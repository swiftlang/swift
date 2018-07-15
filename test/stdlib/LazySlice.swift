// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-run %t/a.out4
// REQUIRES: executable_test

import StdlibUnittest

var tests = TestSuite("LazySlice")

tests.test("CommuteLazyness") {
  let a = [1,2,3].lazy
  let b = a[...]
  var c = b.filter { $0 == 0 }

  expectType(LazyFilterCollection<Slice<LazyCollection<[Int]>>>.self, &c)
}

runAllTests()
