// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=swift-6 -module-cache-path %t | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -module-cache-path %t | %FileCheck %s

// CHECK: struct SimpleArrayWrapper : CxxRandomAccessCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = UnsafePointer<Int32>.Pointee
// CHECK:   typealias RawIterator = UnsafePointer<Int32>
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<SimpleArrayWrapper, UnsafePointer<Int32>.Pointee>
// CHECK:   typealias Iterator = CxxIterator<SimpleArrayWrapper>
// CHECK: }

// CHECK: struct SimpleCollectionNoSubscript : CxxRandomAccessCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = ConstRACIterator.Pointee
// CHECK:   typealias RawIterator = SimpleCollectionNoSubscript.iterator
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<SimpleCollectionNoSubscript, ConstRACIterator.Pointee>
// CHECK:   typealias Iterator = CxxIterator<SimpleCollectionNoSubscript>
// CHECK: }

// CHECK: struct SimpleCollectionReadOnly : CxxRandomAccessCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = ConstRACIteratorRefPlusEq.Pointee
// CHECK:   typealias RawIterator = SimpleCollectionReadOnly.iterator
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<SimpleCollectionReadOnly, ConstRACIteratorRefPlusEq.Pointee>
// CHECK:   typealias Iterator = CxxIterator<SimpleCollectionReadOnly>
// CHECK: }

// CHECK: struct SimpleCollectionReadWrite : CxxMutableRandomAccessCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = ConstRACIterator.Pointee
// CHECK:   typealias RawIterator = SimpleCollectionReadWrite.const_iterator
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<SimpleCollectionReadWrite, ConstRACIterator.Pointee>
// CHECK:   typealias Iterator = CxxIterator<SimpleCollectionReadWrite>
// CHECK:   typealias RawMutableIterator = SimpleCollectionReadWrite.iterator
// CHECK: }

// CHECK: struct HasInheritedTemplatedConstRACIterator<CInt> : CxxRandomAccessCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = InheritedTemplatedConstRACIterator<CInt>.Pointee
// CHECK:   typealias RawIterator = InheritedTemplatedConstRACIterator<CInt>
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<HasInheritedTemplatedConstRACIterator<CInt>, InheritedTemplatedConstRACIterator<CInt>.Pointee>
// CHECK:   typealias Iterator = CxxIterator<HasInheritedTemplatedConstRACIterator<CInt>>
// CHECK: }

// CHECK: struct HasInheritedTemplatedConstRACIteratorOutOfLineOps<CInt> : CxxRandomAccessCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = InheritedTemplatedConstRACIteratorOutOfLineOps<CInt>.Pointee
// CHECK:   typealias RawIterator = InheritedTemplatedConstRACIteratorOutOfLineOps<CInt>
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<HasInheritedTemplatedConstRACIteratorOutOfLineOps<CInt>, InheritedTemplatedConstRACIteratorOutOfLineOps<CInt>.Pointee>
// CHECK:   typealias Iterator = CxxIterator<HasInheritedTemplatedConstRACIteratorOutOfLineOps<CInt>>
// CHECK: }
