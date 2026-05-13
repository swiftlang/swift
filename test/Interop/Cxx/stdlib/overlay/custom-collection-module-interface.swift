// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=swift-6 -module-cache-path %t | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -module-cache-path %t | %FileCheck %s

// CHECK: struct SimpleArrayWrapper : CxxRandomAccessCollection, CxxIterable {
// CHECK:   typealias Element = UnsafePointer<Int32>.Pointee
// CHECK:   typealias RawIterator = UnsafePointer<Int32>
// CHECK:   typealias IterableIterator = CxxIterableIterator<SimpleArrayWrapper>
// CHECK:   typealias Iterator = CxxIterator<SimpleArrayWrapper>
// CHECK: }

// CHECK: struct SimpleCollectionNoSubscript : CxxRandomAccessCollection, CxxIterable {
// CHECK:   typealias Element = ConstRACIterator.Pointee
// CHECK:   typealias RawIterator = SimpleCollectionNoSubscript.iterator
// CHECK:   typealias IterableIterator = CxxIterableIterator<SimpleCollectionNoSubscript>
// CHECK:   typealias Iterator = CxxIterator<SimpleCollectionNoSubscript>
// CHECK: }

// CHECK: struct SimpleCollectionReadOnly : CxxRandomAccessCollection, CxxIterable {
// CHECK:   typealias Element = ConstRACIteratorRefPlusEq.Pointee
// CHECK:   typealias RawIterator = SimpleCollectionReadOnly.iterator
// CHECK:   typealias IterableIterator = CxxIterableIterator<SimpleCollectionReadOnly>
// CHECK:   typealias Iterator = CxxIterator<SimpleCollectionReadOnly>
// CHECK: }

// CHECK: struct SimpleCollectionReadWrite : CxxMutableRandomAccessCollection, CxxIterable {
// CHECK:   typealias Element = ConstRACIterator.Pointee
// CHECK:   typealias RawIterator = SimpleCollectionReadWrite.const_iterator
// CHECK:   typealias IterableIterator = CxxIterableIterator<SimpleCollectionReadWrite>
// CHECK:   typealias Iterator = CxxIterator<SimpleCollectionReadWrite>
// CHECK:   typealias RawMutableIterator = SimpleCollectionReadWrite.iterator
// CHECK: }

// CHECK: struct HasInheritedTemplatedConstRACIterator<CInt> : CxxRandomAccessCollection, CxxIterable {
// CHECK:   typealias Element = InheritedTemplatedConstRACIterator<CInt>.Pointee
// CHECK:   typealias RawIterator = InheritedTemplatedConstRACIterator<CInt>
// CHECK:   typealias IterableIterator = CxxIterableIterator<HasInheritedTemplatedConstRACIterator<CInt>>
// CHECK:   typealias Iterator = CxxIterator<HasInheritedTemplatedConstRACIterator<CInt>>
// CHECK: }

// CHECK: struct HasInheritedTemplatedConstRACIteratorOutOfLineOps<CInt> : CxxRandomAccessCollection, CxxIterable {
// CHECK:   typealias Element = InheritedTemplatedConstRACIteratorOutOfLineOps<CInt>.Pointee
// CHECK:   typealias RawIterator = InheritedTemplatedConstRACIteratorOutOfLineOps<CInt>
// CHECK:   typealias IterableIterator = CxxIterableIterator<HasInheritedTemplatedConstRACIteratorOutOfLineOps<CInt>>
// CHECK:   typealias Iterator = CxxIterator<HasInheritedTemplatedConstRACIteratorOutOfLineOps<CInt>>
// CHECK: }
