// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterator -source-filename=x -I %S/Inputs -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: struct ConstIterator : UnsafeCxxInputIterator {
// CHECK:   func __operatorStar() -> UnsafePointer<CInt>
// CHECK:   func successor() -> ConstIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   static func == (lhs: ConstIterator, other: ConstIterator) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct ConstRACIterator : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> ConstRACIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = CInt
// CHECK:   static func += (lhs: inout ConstRACIterator, v: ConstRACIterator.difference_type)
// CHECK:   static func - (lhs: ConstRACIterator, other: ConstRACIterator) -> CInt
// CHECK:   static func == (lhs: ConstRACIterator, other: ConstRACIterator) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct ConstRACIteratorRefPlusEq : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> ConstRACIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = CInt
// CHECK:   static func += (lhs: inout ConstRACIteratorRefPlusEq, v: ConstRACIteratorRefPlusEq.difference_type)
// CHECK:   static func - (lhs: ConstRACIteratorRefPlusEq, other: ConstRACIteratorRefPlusEq) -> CInt
// CHECK:   static func == (lhs: ConstRACIteratorRefPlusEq, other: ConstRACIteratorRefPlusEq) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct ConstIteratorOutOfLineEq : UnsafeCxxInputIterator {
// CHECK:   func successor() -> ConstIteratorOutOfLineEq
// CHECK:   var pointee: CInt { get }
// CHECK: }
// CHECK: func == (lhs: ConstIteratorOutOfLineEq, rhs: ConstIteratorOutOfLineEq) -> CBool

// CHECK: struct MinimalIterator : UnsafeCxxInputIterator {
// CHECK:   func successor() -> MinimalIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   static func == (lhs: MinimalIterator, other: MinimalIterator) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct ForwardIterator : UnsafeCxxInputIterator {
// CHECK:   func successor() -> ForwardIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   static func == (lhs: ForwardIterator, other: ForwardIterator) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct HasCustomIteratorTag : UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasCustomIteratorTag
// CHECK:   typealias Pointee = CInt
// CHECK:   static func == (lhs: HasCustomIteratorTag, other: HasCustomIteratorTag) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct HasCustomRACIteratorTag : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasCustomRACIteratorTag
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = CInt
// CHECK:   static func += (lhs: inout HasCustomRACIteratorTag, x: CInt)
// CHECK:   static func - (lhs: HasCustomRACIteratorTag, x: HasCustomRACIteratorTag) -> CInt
// CHECK:   static func == (lhs: HasCustomRACIteratorTag, other: HasCustomRACIteratorTag) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct HasCustomInheritedRACIteratorTag : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   struct CustomTag0 {
// CHECK:     init()
// CHECK:   }
// CHECK:   typealias CustomTag1 = HasCustomInheritedRACIteratorTag.CustomTag0
// CHECK:   struct CustomTag2 {
// CHECK:     init()
// CHECK:   }
// CHECK:   typealias CustomTag3 = HasCustomInheritedRACIteratorTag.CustomTag2
// CHECK:   typealias CustomTag4 = HasCustomInheritedRACIteratorTag.CustomTag3
// CHECK:   typealias iterator_category = HasCustomInheritedRACIteratorTag.CustomTag4
// CHECK:   func successor() -> HasCustomInheritedRACIteratorTag
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = CInt
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct HasCustomIteratorTagInline : UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasCustomIteratorTagInline
// CHECK:   typealias Pointee = CInt
// CHECK:   static func == (lhs: HasCustomIteratorTagInline, other: HasCustomIteratorTagInline) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct HasTypedefIteratorTag : UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasTypedefIteratorTag
// CHECK:   typealias Pointee = CInt
// CHECK:   static func == (lhs: HasTypedefIteratorTag, other: HasTypedefIteratorTag) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct MutableRACIterator : UnsafeCxxMutableRandomAccessIterator, UnsafeCxxMutableInputIterator {
// CHECK:   func successor() -> MutableRACIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = CInt
// CHECK: }

// CHECK: struct DifferentResultsDereferenceOperator : UnsafeCxxMutableInputIterator {
// CHECK:   func __operatorStar() -> UnsafePointer<CInt>
// CHECK:   func successor() -> DifferentResultsDereferenceOperator
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias DereferenceResult = UnsafePointer<CInt>
// CHECK:   static func == (lhs: DifferentResultsDereferenceOperator, other: DifferentResultsDereferenceOperator) -> CBool
// CHECK:   var pointee: CInt
// CHECK: }

// CHECK: struct HasNestedIterator : CxxConvertibleToCollection, CxxIterable {
// CHECK:   struct NestedIterator : UnsafeCxxInputIterator {
// CHECK:     func __operatorStar() -> HasNestedIterator.NestedIterator.reference
// CHECK:     var ptr: UnsafePointer<CInt>!
// CHECK:     func successor() -> HasNestedIterator.NestedIterator
// CHECK:     var pointee: CInt { get }
// CHECK:   }
// CHECK: }

// CHECK: struct NonInlineDereferenceOperator {
// CHECK-NOT:   func __operatorStar()
// CHECK:   func successor() -> NonInlineDereferenceOperator
// CHECK-NOT:   typealias Pointee
// CHECK-NOT:   typealias DereferenceResult
// CHECK-NOT:   var pointee
// CHECK: }

// CHECK: struct NonReferenceDereferenceOperator : UnsafeCxxInputIterator {
// CHECK:   func __operatorStar() -> CInt
// CHECK:   func successor() -> NonReferenceDereferenceOperator
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias DereferenceResult = CInt
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct NoConstDereferenceOperator {
// CHECK:   mutating func __operatorStar() -> UnsafeMutablePointer<CInt>
// CHECK:   static func * (lhs: NoConstDereferenceOperator, other: NoConstDereferenceOperator) -> CInt
// CHECK:   static func == (lhs: NoConstDereferenceOperator, other: NoConstDereferenceOperator) -> CBool
// CHECK:   func successor() -> NoConstDereferenceOperator
// CHECK:   var pointee: CInt { mutating get set }
// CHECK: }

// CHECK: struct ConstRACButNotBorrowingIterator : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func __operatorStar() -> CInt
// CHECK:   func successor() -> ConstRACButNotBorrowingIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias DereferenceResult = CInt
// CHECK:   typealias Distance = CInt
// CHECK:   static func += (lhs: inout ConstRACButNotBorrowingIterator, v: ConstRACButNotBorrowingIterator.difference_type)
// CHECK:   static func - (lhs: ConstRACButNotBorrowingIterator, other: ConstRACButNotBorrowingIterator) -> CInt
// CHECK:   static func == (lhs: ConstRACButNotBorrowingIterator, other: ConstRACButNotBorrowingIterator) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK-NOT: struct HasNoIteratorCategory : UnsafeCxxInputIterator
// CHECK-NOT: struct HasInvalidIteratorCategory : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoEqualEqual : UnsafeCxxInputIterator
// CHECK-NOT: struct HasInvalidEqualEqual : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoIncrementOperator : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoPreIncrementOperator : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoDereferenceOperator : UnsafeCxxInputIterator

// CHECK: struct TemplatedIterator<CInt> : UnsafeCxxInputIterator {
// CHECK:   func successor() -> TemplatedIterator<CInt>
// CHECK:   typealias Pointee = CInt
// CHECK:   static func == (lhs: TemplatedIterator<CInt>, other: TemplatedIterator<CInt>) -> CBool
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct TemplatedIteratorOutOfLineEq<CInt> : UnsafeCxxInputIterator {
// CHECK:   func successor() -> TemplatedIteratorOutOfLineEq<CInt>
// CHECK:   typealias Pointee = CInt
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct TemplatedRACIteratorOutOfLineEq<CInt> : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> TemplatedRACIteratorOutOfLineEq<CInt>
// CHECK:   typealias Pointee = CInt
// CHECK:   typealias Distance = TemplatedRACIteratorOutOfLineEq<CInt>.difference_type
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct BaseIntIterator {
// CHECK: }
// CHECK: struct InheritedConstIterator : UnsafeCxxInputIterator {
// CHECK: }

// CHECK: struct InheritedTemplatedConstIterator<T> {
// CHECK: }
// CHECK: struct InheritedTemplatedConstIterator<CInt> : UnsafeCxxInputIterator {
// CHECK: }

// CHECK: struct InheritedTemplatedConstRACIterator<T> {
// CHECK: }
// CHECK: struct InheritedTemplatedConstRACIterator<CInt> : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK: }

// CHECK: struct InheritedTemplatedConstRACIteratorOutOfLineOps<T> {
// CHECK: }
// CHECK: struct InheritedTemplatedConstRACIteratorOutOfLineOps<CInt> : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK: }

// CHECK: struct InputOutputIterator : UnsafeCxxMutableInputIterator {
// CHECK:   func successor() -> InputOutputIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   var pointee: CInt
// CHECK: }

// CHECK: struct InputOutputConstIterator : UnsafeCxxMutableInputIterator {
// CHECK:   func successor() -> InputOutputConstIterator
// CHECK:   typealias Pointee = CInt
// CHECK:   var pointee: CInt { get nonmutating set }
// CHECK: }
