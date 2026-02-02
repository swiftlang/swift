// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterator -source-filename=x -I %S/Inputs -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: struct ConstIterator : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> ConstIterator
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: ConstIterator, other: ConstIterator) -> Bool
// CHECK: }

// CHECK: struct ConstRACIterator : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> ConstRACIterator
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK:   static func += (lhs: inout ConstRACIterator, v: ConstRACIterator.difference_type)
// CHECK:   static func - (lhs: ConstRACIterator, other: ConstRACIterator) -> Int32
// CHECK:   static func == (lhs: ConstRACIterator, other: ConstRACIterator) -> Bool
// CHECK: }

// CHECK: struct ConstRACIteratorRefPlusEq : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> ConstRACIterator
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK:   static func += (lhs: inout ConstRACIteratorRefPlusEq, v: ConstRACIteratorRefPlusEq.difference_type)
// CHECK:   static func - (lhs: ConstRACIteratorRefPlusEq, other: ConstRACIteratorRefPlusEq) -> Int32
// CHECK:   static func == (lhs: ConstRACIteratorRefPlusEq, other: ConstRACIteratorRefPlusEq) -> Bool
// CHECK: }

// CHECK: struct ConstIteratorOutOfLineEq : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> ConstIteratorOutOfLineEq
// CHECK: }
// CHECK: func == (lhs: ConstIteratorOutOfLineEq, rhs: ConstIteratorOutOfLineEq) -> Bool

// CHECK: struct MinimalIterator : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> MinimalIterator
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: MinimalIterator, other: MinimalIterator) -> Bool
// CHECK: }

// CHECK: struct ForwardIterator : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> ForwardIterator
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: ForwardIterator, other: ForwardIterator) -> Bool
// CHECK: }

// CHECK: struct HasCustomIteratorTag : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> HasCustomIteratorTag
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: HasCustomIteratorTag, other: HasCustomIteratorTag) -> Bool
// CHECK: }

// CHECK: struct HasCustomRACIteratorTag : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> HasCustomRACIteratorTag
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK:   static func += (lhs: inout HasCustomRACIteratorTag, x: Int32)
// CHECK:   static func - (lhs: HasCustomRACIteratorTag, x: HasCustomRACIteratorTag) -> Int32
// CHECK:   static func == (lhs: HasCustomRACIteratorTag, other: HasCustomRACIteratorTag) -> Bool
// CHECK: }

// CHECK: struct HasCustomInheritedRACIteratorTag : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> HasCustomInheritedRACIteratorTag
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
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
// CHECK: }

// CHECK: struct HasCustomIteratorTagInline : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> HasCustomIteratorTagInline
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: HasCustomIteratorTagInline, other: HasCustomIteratorTagInline) -> Bool
// CHECK: }

// CHECK: struct HasTypedefIteratorTag : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> HasTypedefIteratorTag
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: HasTypedefIteratorTag, other: HasTypedefIteratorTag) -> Bool
// CHECK: }

// CHECK: struct MutableRACIterator : UnsafeCxxMutableRandomAccessIterator, UnsafeCxxMutableInputIterator {
// CHECK:   var pointee: Int32
// CHECK:   func successor() -> MutableRACIterator
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK: }

// CHECK-NOT: struct HasNoIteratorCategory : UnsafeCxxInputIterator
// CHECK-NOT: struct HasInvalidIteratorCategory : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoEqualEqual : UnsafeCxxInputIterator
// CHECK-NOT: struct HasInvalidEqualEqual : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoIncrementOperator : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoPreIncrementOperator : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoDereferenceOperator : UnsafeCxxInputIterator

// CHECK: struct TemplatedIterator<CInt> : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> TemplatedIterator<CInt>
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: TemplatedIterator<CInt>, other: TemplatedIterator<CInt>) -> Bool
// CHECK: }

// CHECK: struct TemplatedIteratorOutOfLineEq<CInt> : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> TemplatedIteratorOutOfLineEq<CInt>
// CHECK:   typealias Pointee = Int32
// CHECK: }

// CHECK: struct TemplatedRACIteratorOutOfLineEq<CInt> : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> TemplatedRACIteratorOutOfLineEq<CInt>
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = TemplatedRACIteratorOutOfLineEq<CInt>.difference_type
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
// CHECK:   var pointee: Int32
// CHECK:   func successor() -> InputOutputIterator
// CHECK:   typealias Pointee = Int32
// CHECK: }

// CHECK: struct InputOutputConstIterator : UnsafeCxxMutableInputIterator {
// CHECK:   var pointee: Int32 { get nonmutating set }
// CHECK:   func successor() -> InputOutputConstIterator
// CHECK:   typealias Pointee = Int32
// CHECK: }
