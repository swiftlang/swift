// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -Ounchecked
//
// RUN: %target-run %t/a.out

import StdlibUnittest

var DictionaryUnchecked = TestSuite("DictionaryUnchecked")

DictionaryUnchecked.test("noCseOnInit") {

  @inline(never)
  func createDict() -> Dictionary<Int, Bool> {
    // CSE should not be able to combine both Dictionary.init() calls.
    // This did happen and resulted in a crash because Dictionary.init()
    // was defined with @effects(readnone).
    // But this was wrong because it actually reads the array buffer (from
    // the literal).
    var Dict: Dictionary<Int, Bool> = [:]
    Dict = [:]
    Dict[0] = true
	return Dict
  }

  let Dict = createDict()
  expectTrue(Dict[0]!)
}

runAllTests()

