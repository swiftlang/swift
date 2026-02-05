// RUN: %target-swift-ide-test -print-module -module-to-print=CustomBorrowingSequence -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop -module-cache-path %t -I %swift_src_root/lib/ClangImporter/SwiftBridging | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomBorrowingSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=swift-6 -module-cache-path %t -I %swift_src_root/lib/ClangImporter/SwiftBridging | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomBorrowingSequence -source-filename=x -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -module-cache-path %t -I %swift_src_root/lib/ClangImporter/SwiftBridging | %FileCheck %s

// CHECK:     struct SimpleNonCopyableSequence : CxxBorrowingSequence {
// CHECK:       typealias Element = ConstIterator.Pointee
// CHECK:       typealias RawIterator = ConstIterator
// CHECK:       typealias BorrowingIterator = CxxBorrowingIterator<SimpleNonCopyableSequence, ConstIterator.Pointee>
// CHECK-NOT-TODO:   typealias Iterator
// CHECK:     }

// CHECK:     struct SimpleConditionallyCopyableSequence<CInt> : CxxConvertibleToCollection, CxxBorrowingSequence {
// CHECK:       typealias Element = ConstIterator.Pointee
// CHECK:       typealias RawIterator = ConstIterator
// CHECK:       typealias BorrowingIterator = CxxBorrowingIterator<SimpleConditionallyCopyableSequence<CInt>, ConstIterator.Pointee>
// CHECK:       typealias Iterator = CxxIterator<SimpleConditionallyCopyableSequence<CInt>>
// CHECK:     }

// CHECK:     struct SimpleConditionallyCopyableSequence<NonCop> : CxxBorrowingSequence {
// CHECK:       typealias Element = ConstIterator.Pointee
// CHECK:       typealias RawIterator = ConstIterator
// CHECK:       typealias BorrowingIterator = CxxBorrowingIterator<SimpleConditionallyCopyableSequence<NonCop>, ConstIterator.Pointee>
// CHECK-NOT-TODO:   typealias Iterator
// CHECK:     }

// CHECK: struct NonReferenceDereferenceOperatorSequence {
// CHECK:   typealias Element = NonReferenceDereferenceOperator.Pointee
// CHECK:   typealias RawIterator = NonReferenceDereferenceOperator
// CHECK:   typealias BorrowingIterator = CxxBorrowingIterator<NonReferenceDereferenceOperatorSequence, NonReferenceDereferenceOperator.Pointee>
// CHECK:   typealias Iterator = CxxIterator<NonReferenceDereferenceOperatorSequence>
// CHECK: }
