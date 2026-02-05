// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=swift-6 -module-cache-path %t | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -module-cache-path %t | %FileCheck %s

// CHECK: struct SimpleSequence : CxxConvertibleToCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = ConstIterator.Pointee
// CHECK:   typealias RawIterator = ConstIterator
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<SimpleSequence, ConstIterator.Pointee>
// CHECK:   typealias Iterator = CxxIterator<SimpleSequence>
// CHECK: }

// CHECK: struct SimpleSequenceWithOutOfLineEqualEqual : CxxConvertibleToCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = ConstIteratorOutOfLineEq.Pointee
// CHECK:   typealias RawIterator = ConstIteratorOutOfLineEq
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<SimpleSequenceWithOutOfLineEqualEqual, ConstIteratorOutOfLineEq.Pointee>
// CHECK:   typealias Iterator = CxxIterator<SimpleSequenceWithOutOfLineEqualEqual>
// CHECK: }

// CHECK: struct SimpleArrayWrapperNullableIterators : CxxConvertibleToCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = Optional<UnsafePointer<Int32>>.Pointee
// CHECK:   typealias RawIterator = UnsafePointer<Int32>?
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<SimpleArrayWrapperNullableIterators, Optional<UnsafePointer<Int32>>.Pointee>
// CHECK:   typealias Iterator = CxxIterator<SimpleArrayWrapperNullableIterators>
// CHECK: }

// CHECK: struct SimpleEmptySequence : CxxConvertibleToCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = Optional<UnsafePointer<Int32>>.Pointee
// CHECK:   typealias RawIterator = UnsafePointer<Int32>?
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<SimpleEmptySequence, Optional<UnsafePointer<Int32>>.Pointee>
// CHECK:   typealias Iterator = CxxIterator<SimpleEmptySequence>
// CHECK: }

// CHECK: struct HasMutatingBeginEnd {
// CHECK-NOT:   typealias Element = ConstIterator.Pointee
// CHECK-NOT:   typealias RawIterator = ConstIterator
// CHECK-NOT:   typealias BorrowingIterator
// CHECK-NOT:   typealias Iterator = CxxIterator<HasMutatingBeginEnd>
// CHECK: }

// CHECK: struct HasNoBeginMethod {
// CHECK-NOT:   typealias Element
// CHECK-NOT:   typealias RawIterator
// CHECK-NOT:   typealias BorrowingIterator
// CHECK-NOT:   typealias Iterator
// CHECK: }
// CHECK: struct HasNoEndMethod {
// CHECK-NOT:   typealias Element
// CHECK-NOT:   typealias RawIterator
// CHECK-NOT:   typealias BorrowingIterator
// CHECK-NOT:   typealias Iterator
// CHECK: }
// CHECK: struct HasBeginEndTypeMismatch {
// CHECK-NOT:   typealias Element
// CHECK-NOT:   typealias RawIterator
// CHECK-NOT:   typealias BorrowingIterator
// CHECK-NOT:   typealias Iterator
// CHECK: }
// CHECK: struct HasBeginEndReturnNonIterators {
// CHECK-NOT:   typealias Element
// CHECK-NOT:   typealias RawIterator
// CHECK-NOT:   typealias BorrowingIterator
// CHECK-NOT:   typealias Iterator
// CHECK: }
// CHECK: struct HasTemplatedIterator<CInt, NoDefinition<CInt>> {
// CHECK-NOT:   typealias Element
// CHECK-NOT:   typealias RawIterator
// CHECK-NOT:   typealias BorrowingIterator
// CHECK-NOT:   typealias Iterator
// CHECK: }
// CHECK: typealias HasUninstantiatableIterator = HasTemplatedIterator<CInt, NoDefinition<CInt>>

// CHECK: struct HasInputOutputConstIterator : CxxConvertibleToCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = InputOutputConstIterator.Pointee
// CHECK:   typealias RawIterator = HasInputOutputConstIterator.iterator
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<HasInputOutputConstIterator, InputOutputConstIterator.Pointee>
// CHECK:   typealias Iterator = CxxIterator<HasInputOutputConstIterator>
// CHECK: }
