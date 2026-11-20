// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterator -source-filename=x -I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 | %FileCheck %s

// Ubuntu 20.04 ships with an old version of libstdc++, which does not provide
// std::contiguous_iterator_tag from C++20.
// UNSUPPORTED: LinuxDistribution=ubuntu-20.04
// UNSUPPORTED: LinuxDistribution=amzn-2

// CHECK: struct ConstContiguousIterator : UnsafeCxxContiguousIterator, UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> ConstContiguousIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = CInt
// CHECK:   var pointee: CInt
// CHECK: }

// CHECK: struct HasCustomContiguousIteratorTag : UnsafeCxxContiguousIterator, UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasCustomContiguousIteratorTag
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = CInt
// CHECK:   var pointee: CInt
// CHECK: }

// CHECK: struct MutableContiguousIterator : UnsafeCxxMutableContiguousIterator, UnsafeCxxMutableRandomAccessIterator, UnsafeCxxMutableInputIterator {
// CHECK:   func successor() -> MutableContiguousIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = CInt
// CHECK:   var pointee: CInt
// CHECK: }

// CHECK: struct HasNoContiguousIteratorConcept : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasNoContiguousIteratorConcept
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = CInt
// CHECK:   var pointee: CInt
// CHECK: }

// CHECK: struct InheritedFromBaseWithIteratorConcept : UnsafeCxxInputIterator {
// CHECK: }

// CHECK: struct ReturnsInheritedFromBaseWithIteratorConcept {
// CHECK:   @available(*, deprecated, renamed: "get()")
// CHECK:   func __getUnsafe() -> InheritedFromBaseWithIteratorConcept
// CHECK:   func get() -> InheritedFromBaseWithIteratorConcept
// CHECK: }

// CHECK: struct HasOnlyIteratorConcept : UnsafeCxxInputIterator {
// CHECK:   func __operatorStar() -> UnsafePointer<CInt>
// CHECK:   func successor() -> HasOnlyIteratorConcept
// CHECK:   typealias Pointee = CInt
// CHECK:   static func == (lhs: HasOnlyIteratorConcept, other: HasOnlyIteratorConcept) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct ReturnsOnlyIteratorConcept {
// CHECK:   @available(*, deprecated, renamed: "get()")
// CHECK:   func __getUnsafe() -> HasOnlyIteratorConcept
// CHECK:   func get() -> HasOnlyIteratorConcept
// CHECK: }
