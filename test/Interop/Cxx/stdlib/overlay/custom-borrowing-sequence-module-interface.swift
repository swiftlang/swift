// RUN: %target-swift-ide-test -print-module -module-to-print=CustomBorrowingSequence -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop -module-cache-path %t -I %swift_src_root/lib/ClangImporter/SwiftBridging | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomBorrowingSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=swift-6 -module-cache-path %t -I %swift_src_root/lib/ClangImporter/SwiftBridging | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomBorrowingSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -module-cache-path %t -I %swift_src_root/lib/ClangImporter/SwiftBridging | %FileCheck %s

// CHECK:     struct SimpleNonCopyableSequence : ~Copyable, CxxBorrowingSequence {
// CHECK:       typealias Element = ConstIterator.Pointee
// CHECK:       typealias RawIterator = ConstIterator
// CHECK:       typealias BorrowingIterator = CxxBorrowingIterator<SimpleNonCopyableSequence>
// CHECK-NOT-TODO:   typealias Iterator
// CHECK:     }

// CHECK:     struct SimpleConditionallyCopyableSequence<CInt> : CxxConvertibleToCollection, CxxBorrowingSequence {
// CHECK:       typealias Element = ConstIterator.Pointee
// CHECK:       typealias RawIterator = ConstIterator
// CHECK:       typealias BorrowingIterator = CxxBorrowingIterator<SimpleConditionallyCopyableSequence<CInt>>
// CHECK:       typealias Iterator = CxxIterator<SimpleConditionallyCopyableSequence<CInt>>
// CHECK:     }

// CHECK:     struct SimpleConditionallyCopyableSequence<NonCop> : ~Copyable, CxxBorrowingSequence {
// CHECK:       typealias Element = ConstIterator.Pointee
// CHECK:       typealias RawIterator = ConstIterator
// CHECK:       typealias BorrowingIterator = CxxBorrowingIterator<SimpleConditionallyCopyableSequence<NonCop>>
// CHECK-NOT-TODO:   typealias Iterator
// CHECK:     }

// CHECK: struct NonReferenceDereferenceOperatorSequence : ~Copyable {
// CHECK:   typealias Element = NonReferenceDereferenceOperator.Pointee
// CHECK:   typealias RawIterator = NonReferenceDereferenceOperator
// CHECK:   typealias Iterator = CxxIterator<NonReferenceDereferenceOperatorSequence>
// CHECK: }

// CHECK: struct NonInlineDereferenceOperatorSequence {
// CHECK-NOT:   typealias Element
// CHECK-NOT:   typealias RawIterator
// CHECK-NOT:   typealias Iterator
// CHECK: }

// CHECK: struct NoConstDereferenceOperatorSequence {
// CHECK-NOT:   typealias Element
// CHECK-NOT:   typealias RawIterator
// CHECK-NOT:   typealias Iterator
// CHECK: }

// CHECK: struct DifferentResultsDereferenceOperatorSequence : CxxConvertibleToCollection, CxxBorrowingSequence {
// CHECK:   typealias Element = DifferentResultsDereferenceOperator.Pointee
// CHECK:   typealias RawIterator = DifferentResultsDereferenceOperator
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<DifferentResultsDereferenceOperatorSequence>
// CHECK:   typealias Iterator = CxxIterator<DifferentResultsDereferenceOperatorSequence>
// CHECK: }

// CHECK: struct ConstRACButNotBorrowingIteratorSequence : CxxRandomAccessCollection {
// CHECK:   typealias Element = ConstRACButNotBorrowingIterator.Pointee
// CHECK:   typealias RawIterator = ConstRACButNotBorrowingIterator
// CHECK:   typealias Iterator = CxxIterator<ConstRACButNotBorrowingIteratorSequence>
// CHECK:   typealias Index = Int
// CHECK:   typealias Indices = Range<Int>
// CHECK:   typealias SubSequence = Slice<ConstRACButNotBorrowingIteratorSequence>
// CHECK: }
