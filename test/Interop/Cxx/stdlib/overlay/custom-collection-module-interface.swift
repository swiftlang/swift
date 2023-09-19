// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s

// CHECK: struct SimpleArrayWrapper : CxxRandomAccessCollection {
// CHECK:   typealias Element = UnsafePointer<Int32>.Pointee
// CHECK:   typealias Iterator = CxxIterator<SimpleArrayWrapper>
// CHECK:   typealias RawIterator = UnsafePointer<Int32>
// CHECK: }

// CHECK: struct SimpleCollectionNoSubscript : CxxRandomAccessCollection {
// CHECK:   typealias Element = ConstRACIterator.Pointee
// CHECK:   typealias Iterator = CxxIterator<SimpleCollectionNoSubscript>
// CHECK:   typealias RawIterator = SimpleCollectionNoSubscript.iterator
// CHECK: }

// CHECK: struct SimpleCollectionReadOnly : CxxRandomAccessCollection {
// CHECK:   typealias Element = ConstRACIteratorRefPlusEq.Pointee
// CHECK:   typealias Iterator = CxxIterator<SimpleCollectionReadOnly>
// CHECK:   typealias RawIterator = SimpleCollectionReadOnly.iterator
// CHECK: }

// CHECK: struct HasInheritedTemplatedConstRACIterator<CInt> : CxxRandomAccessCollection {
// CHECK:   typealias Element = InheritedTemplatedConstRACIterator<CInt>.Pointee
// CHECK:   typealias Iterator = CxxIterator<HasInheritedTemplatedConstRACIterator<CInt>>
// CHECK:   typealias RawIterator = InheritedTemplatedConstRACIterator<CInt>
// CHECK: }

// CHECK: struct HasInheritedTemplatedConstRACIteratorOutOfLineOps<CInt> : CxxRandomAccessCollection {
// CHECK:   typealias Element = InheritedTemplatedConstRACIteratorOutOfLineOps<CInt>.Pointee
// CHECK:   typealias Iterator = CxxIterator<HasInheritedTemplatedConstRACIteratorOutOfLineOps<CInt>>
// CHECK:   typealias RawIterator = InheritedTemplatedConstRACIteratorOutOfLineOps<CInt>
// CHECK: }
