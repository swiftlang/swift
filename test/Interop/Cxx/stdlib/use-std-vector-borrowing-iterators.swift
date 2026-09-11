// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++20)

// Also test this with a bridging header instead of the StdVector module.
// RUN: %empty-directory(%t2)
// RUN: cp %S/Inputs/std-vector.h %t2/std-vector-bridging-header.h
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-vector-bridging-header.h -cxx-interoperability-mode=default -Xcc -std=c++20)

// Ubuntu 20.04 ships with an old version of libstdc++, which does not provide
// std::contiguous_iterator_tag from C++20.
// REQUIRES: OS=macosx || OS=windows-msvc

// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

import StdlibUnittest
#if !BRIDGING_HEADER
import StdVector
#endif
import CxxStdlib

// This test makes sure that, after C++20 and on platforms where std::contiguous_iterator_tag is available, 
// we pick up the conformance to `UnsafeCxxContiguousIterator`. 
// This conformances allows `nextSpan()` to return a span containing all std::vector elements, 
// instead of a single element (like it does for C++ iterators that conform only to `UnsafeCxxInputIterator`).
var StdVectorBorrowingIteratorTestSuite = TestSuite("StdVectorBorrowingIterator")

StdVectorBorrowingIteratorTestSuite.test("VecOfInt has contiguous iterator").require(.stdlib_6_4).code {
    guard #available(SwiftStdlib 6.4, *) else { return }
    let arr : [Int32] = [1, 2, 3, 4, 5]
    let v = Vector(arr)
    expectEqual(v.size(), 5)
    var iterator = v.makeBorrowingIterator()
    var counter = 0
    while true {
        let span = iterator.nextSpan()
        if (span.count == 0) { break }
        expectEqual(span.count, 5)
        counter += 1
    }
    expectEqual(counter, 1)
}

StdVectorBorrowingIteratorTestSuite.test("VectorOfNonCopyable has contiguous iterator").require(.stdlib_6_4).code {
    guard #available(SwiftStdlib 6.4, *) else { return }
    let v = makeVectorOfNonCopyable()
    expectEqual(v.size(), 3)
    var iterator = v.makeBorrowingIterator()
    var counter = 0
    while true {
        let span = iterator.nextSpan()
        if (span.count == 0) { break }
        expectEqual(span.count, 3)
        counter += 1
    }
    expectEqual(counter, 1)
}

runAllTests()
