// RUN: %target-swift-ide-test -print-module -module-to-print=CustomSequence -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s

// CHECK: import Cxx

// CHECK: struct ConstIterator : UnsafeCxxInputIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   func successor() -> ConstIterator
// CHECK:   typealias Pointee = Int32
// CHECK:   static func == (lhs: ConstIterator, other: ConstIterator) -> Bool
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

// CHECK-NOT: struct HasNoIteratorCategory : UnsafeCxxInputIterator
// CHECK-NOT: struct HasInvalidIteratorCategory : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoEqualEqual : UnsafeCxxInputIterator
// CHECK-NOT: struct HasInvalidEqualEqual : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoIncrementOperator : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoPreIncrementOperator : UnsafeCxxInputIterator
// CHECK-NOT: struct HasNoDereferenceOperator : UnsafeCxxInputIterator
