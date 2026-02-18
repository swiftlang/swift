// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xcc -std=c++20)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6 -Xcc -std=c++20)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// Also test this with a bridging header instead of the StdVector module.
// RUN: %empty-directory(%t2)
// RUN: cp %S/Inputs/std-vector.h %t2/std-vector-bridging-header.h
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-vector-bridging-header.h -Xfrontend -enable-experimental-cxx-interop -Xcc -std=c++20)
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-vector-bridging-header.h -cxx-interoperability-mode=swift-6 -Xcc -std=c++20)
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-vector-bridging-header.h -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// Ubuntu 20.04 ships with an old version of libstdc++, which does not provide
// std::contiguous_iterator_tag from C++20.
// REQUIRES: OS=macosx || OS=windows-msvc

// REQUIRES: executable_test

import StdlibUnittest
#if !BRIDGING_HEADER
import StdVector
#endif
import CxxStdlib

var StdVectorBorrowingIteratorTestSuite = TestSuite("StdVectorBorrowingIterator")

StdVectorBorrowingIteratorTestSuite.test("VecOfInt has contiguous iterator").require(.stdlib_6_3).code {
    guard #available(SwiftStdlib 6.3, *) else { return }
    let arr : [Int32] = [1, 2, 3, 4, 5]
    let v = Vector(arr)
    expectEqual(v.size(), 5)
    var iterator = v.makeBorrowingIterator()
    var counter = 0
    while true {
        let span = iterator._nextSpan()
        if (span.count == 0) { break }
        expectEqual(span.count, 5)
        counter += 1
    }
    expectEqual(counter, 1)
}

StdVectorBorrowingIteratorTestSuite.test("VectorOfNonCopyable has contiguous iterator").require(.stdlib_6_3).code {
    guard #available(SwiftStdlib 6.3, *) else { return }
    let v = makeVectorOfNonCopyable()
    expectEqual(v.size(), 3)
    var iterator = v.makeBorrowingIterator()
    var counter = 0
    while true {
        let span = iterator._nextSpan()
        if (span.count == 0) { break }
        expectEqual(span.count, 3)
        counter += 1
    }
    expectEqual(counter, 1)
}

runAllTests()
