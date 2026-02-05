// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -I %swift_src_root/lib/ClangImporter/SwiftBridging)
//
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu || OS=freebsd

import StdlibUnittest
import CustomBorrowingSequence

var CxxBorrowingSequenceTestSuite = TestSuite("CxxSequence")

CxxBorrowingSequenceTestSuite.test("SimpleNonCopyableSequence as Swift.BorrowingSequence") {
  let seq = SimpleNonCopyableSequence()

  var iterator = seq.makeBorrowingIterator()
    var counter = 0
    while true {
        let span = iterator.nextSpan()
        if (span.count == 0) { break }
        for i in 0..<span.count {
          // TODO span[i] makes the compiler crash 
            counter += 1
        }
    }
    expectEqual(counter, 4)
}

runAllTests()
