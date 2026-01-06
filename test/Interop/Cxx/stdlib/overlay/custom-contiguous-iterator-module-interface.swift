// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterator -source-filename=x -I %S/Inputs -cxx-interoperability-mode=swift-6 -Xcc -std=c++20 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterator -source-filename=x -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20 | %FileCheck %s 

// Ubuntu 20.04 ships with an old version of libstdc++, which does not provide
// std::contiguous_iterator_tag from C++20.
// UNSUPPORTED: LinuxDistribution=ubuntu-20.04
// UNSUPPORTED: LinuxDistribution=amzn-2

// CHECK: struct ConstContiguousIterator : UnsafeCxxContiguousIterator, UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> ConstContiguousIterator
// CHECK:   var pointee: Int32
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK: }

// CHECK: struct HasCustomContiguousIteratorTag : UnsafeCxxContiguousIterator, UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasCustomContiguousIteratorTag
// CHECK:   var pointee: Int32
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK: }

// CHECK: struct MutableContiguousIterator : UnsafeCxxMutableContiguousIterator, UnsafeCxxMutableRandomAccessIterator, UnsafeCxxMutableInputIterator {
// CHECK:   func successor() -> MutableContiguousIterator
// CHECK:   var pointee: Int32
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK: }

// CHECK: struct HasNoContiguousIteratorConcept : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasNoContiguousIteratorConcept
// CHECK:   var pointee: Int32
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK: }
