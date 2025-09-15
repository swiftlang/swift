// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterator -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterator -source-filename=x -I %S/Inputs -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=CustomIterator -source-filename=x -I %S/Inputs -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: struct ConstIterator : UnsafeCxxInputIterator {
// CHECK:   func successor() -> ConstIterator
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: ConstIterator, other: ConstIterator) -> Bool
// CHECK: }

// CHECK: struct ConstRACIterator : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> ConstRACIterator
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK:   static func += (lhs: inout ConstRACIterator, v: ConstRACIterator.difference_type)
// CHECK:   static func - (lhs: ConstRACIterator, other: ConstRACIterator) -> Int32
// CHECK:   static func == (lhs: ConstRACIterator, other: ConstRACIterator) -> Bool
// CHECK: }

// CHECK: struct ConstRACIteratorRefPlusEq : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> ConstRACIterator
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK:   static func += (lhs: inout ConstRACIteratorRefPlusEq, v: ConstRACIteratorRefPlusEq.difference_type)
// CHECK:   static func - (lhs: ConstRACIteratorRefPlusEq, other: ConstRACIteratorRefPlusEq) -> Int32
// CHECK:   static func == (lhs: ConstRACIteratorRefPlusEq, other: ConstRACIteratorRefPlusEq) -> Bool
// CHECK: }

// CHECK: struct ConstIteratorOutOfLineEq : UnsafeCxxInputIterator {
// CHECK:   func successor() -> ConstIteratorOutOfLineEq
// CHECK:   var pointee: Int32 { get }
// CHECK: }
// CHECK: func == (lhs: ConstIteratorOutOfLineEq, rhs: ConstIteratorOutOfLineEq) -> Bool

// CHECK: struct MinimalIterator : UnsafeCxxInputIterator {
// CHECK:   func successor() -> MinimalIterator
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: MinimalIterator, other: MinimalIterator) -> Bool
// CHECK: }

// CHECK: struct ForwardIterator : UnsafeCxxInputIterator {
// CHECK:   func successor() -> ForwardIterator
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: ForwardIterator, other: ForwardIterator) -> Bool
// CHECK: }

// CHECK: struct HasCustomIteratorTag : UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasCustomIteratorTag
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: HasCustomIteratorTag, other: HasCustomIteratorTag) -> Bool
// CHECK: }

// CHECK: struct HasCustomRACIteratorTag : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasCustomRACIteratorTag
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   typealias Distance = Int32
// CHECK:   static func += (lhs: inout HasCustomRACIteratorTag, x: Int32)
// CHECK:   static func - (lhs: HasCustomRACIteratorTag, x: HasCustomRACIteratorTag) -> Int32
// CHECK:   static func == (lhs: HasCustomRACIteratorTag, other: HasCustomRACIteratorTag) -> Bool
// CHECK: }

// CHECK: struct HasCustomInheritedRACIteratorTag : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasCustomInheritedRACIteratorTag
// CHECK:   var pointee: Int32 { get }
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
// CHECK:   func successor() -> HasCustomIteratorTagInline
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: HasCustomIteratorTagInline, other: HasCustomIteratorTagInline) -> Bool
// CHECK: }

// CHECK: struct HasTypedefIteratorTag : UnsafeCxxInputIterator {
// CHECK:   func successor() -> HasTypedefIteratorTag
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: HasTypedefIteratorTag, other: HasTypedefIteratorTag) -> Bool
// CHECK: }

// CHECK: struct MutableRACIterator : UnsafeCxxMutableRandomAccessIterator, UnsafeCxxMutableInputIterator {
// CHECK:   func successor() -> MutableRACIterator
// CHECK:   var pointee: Int32
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
// CHECK:   func successor() -> TemplatedIterator<CInt>
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: TemplatedIterator<CInt>, other: TemplatedIterator<CInt>) -> Bool
// CHECK: }

// CHECK: struct TemplatedIteratorOutOfLineEq<CInt> : UnsafeCxxInputIterator {
// CHECK:   func successor() -> TemplatedIteratorOutOfLineEq<CInt>
// CHECK:   var pointee: Int32 { get }
// CHECK:   typealias Pointee = Int32
// CHECK: }

// CHECK: struct TemplatedRACIteratorOutOfLineEq<CInt> : UnsafeCxxRandomAccessIterator, UnsafeCxxInputIterator {
// CHECK:   func successor() -> TemplatedRACIteratorOutOfLineEq<CInt>
// CHECK:   var pointee: Int32 { get }
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
// CHECK:   func successor() -> InputOutputIterator
// CHECK:   var pointee: Int32
// CHECK:   typealias Pointee = Int32
// CHECK: }

// CHECK: struct InputOutputConstIterator : UnsafeCxxMutableInputIterator {
// CHECK:   func successor() -> InputOutputConstIterator
// CHECK:   var pointee: Int32 { get nonmutating set }
// CHECK:   typealias Pointee = Int32
// CHECK: }
