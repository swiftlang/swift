// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterable -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop -module-cache-path %t -I %swift_src_root/lib/ClangImporter/SwiftBridging | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterable -source-filename=x -I %S/Inputs -cxx-interoperability-mode=swift-6 -module-cache-path %t -I %swift_src_root/lib/ClangImporter/SwiftBridging | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterable -source-filename=x -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -module-cache-path %t -I %swift_src_root/lib/ClangImporter/SwiftBridging | %FileCheck %s

// CHECK:     struct SimpleNonCopyableSequence : ~Copyable, CxxIterable {
// CHECK:       typealias Element = ConstIterator.Pointee
// CHECK:       typealias RawIterator = ConstIterator
// CHECK:       typealias IterableIterator = CxxIterableIterator<SimpleNonCopyableSequence>
// CHECK-NOT-TODO:   typealias Iterator
// CHECK:     }

// CHECK:     struct SimpleConditionallyCopyableSequence<CInt> : CxxConvertibleToCollection, CxxIterable {
// CHECK:       typealias Element = ConstIterator.Pointee
// CHECK:       typealias RawIterator = ConstIterator
// CHECK:       typealias IterableIterator = CxxIterableIterator<SimpleConditionallyCopyableSequence<CInt>>
// CHECK:       typealias Iterator = CxxIterator<SimpleConditionallyCopyableSequence<CInt>>
// CHECK:     }

// CHECK:     struct SimpleConditionallyCopyableSequence<NonCop> : ~Copyable, CxxIterable {
// CHECK:       typealias Element = ConstIterator.Pointee
// CHECK:       typealias RawIterator = ConstIterator
// CHECK:       typealias IterableIterator = CxxIterableIterator<SimpleConditionallyCopyableSequence<NonCop>>
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

// CHECK: struct DifferentResultsDereferenceOperatorSequence : CxxConvertibleToCollection, CxxIterable {
// CHECK:   typealias Element = DifferentResultsDereferenceOperator.Pointee
// CHECK:   typealias RawIterator = DifferentResultsDereferenceOperator
// CHECK:   typealias IterableIterator = CxxIterableIterator<DifferentResultsDereferenceOperatorSequence>
// CHECK:   typealias Iterator = CxxIterator<DifferentResultsDereferenceOperatorSequence>
// CHECK: }

// CHECK: struct ConstRACButNotIterableIteratorSequence : CxxRandomAccessCollection {
// CHECK:   typealias Element = ConstRACButNotIterableIterator.Pointee
// CHECK:   typealias RawIterator = ConstRACButNotIterableIterator
// CHECK:   typealias Iterator = CxxIterator<ConstRACButNotIterableIteratorSequence>
// CHECK:   typealias Index = Int
// CHECK:   typealias Indices = Range<Int>
// CHECK:   typealias SubSequence = Slice<ConstRACButNotIterableIteratorSequence>
// CHECK: }
