// RUN: %target-swift-ide-test -print-module -module-to-print=ScopedEnums -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: enum ScopedEnumDefined : Int32 {
// CHECK:   init?(rawValue: Int32)
// CHECK:   var rawValue: Int32 { get }
// CHECK:   typealias RawValue = Int32
// CHECK:   case x
// CHECK:   case y
// CHECK: }

// CHECK: enum ScopedEnumBasic : Int32 {
// CHECK:   init?(rawValue: Int32)
// CHECK:   var rawValue: Int32 { get }
// CHECK:   typealias RawValue = Int32
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }

// CHECK: enum ScopedEnumCharDefined : CChar {
// CHECK:   init?(rawValue: CChar)
// CHECK:   var rawValue: CChar { get }
// CHECK:   typealias RawValue = CChar
// CHECK:   case x
// CHECK:   case y
// CHECK: }

// CHECK: enum ScopedEnumUnsignedDefined : UInt32 {
// CHECK:   init?(rawValue: UInt32)
// CHECK:   var rawValue: UInt32 { get }
// CHECK:   typealias RawValue = UInt32
// CHECK:   case x
// CHECK:   case y
// CHECK: }

// CHECK: enum ScopedEnumUnsignedLongDefined : [[UINT_T:UInt|UInt32]] {
// CHECK:   init?(rawValue: [[UINT_T]])
// CHECK:   var rawValue: [[UINT_T]] { get }
// CHECK:   typealias RawValue = [[UINT_T]]
// CHECK:   case x
// CHECK:   case y
// CHECK: }

// CHECK: enum ScopedEnumChar : CChar {
// CHECK:   init?(rawValue: CChar)
// CHECK:   var rawValue: CChar { get }
// CHECK:   typealias RawValue = CChar
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }

// CHECK: enum ScopedEnumUnsigned : UInt32 {
// CHECK:   init?(rawValue: UInt32)
// CHECK:   var rawValue: UInt32 { get }
// CHECK:   typealias RawValue = UInt32
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }

// CHECK: enum ScopedEnumUnsignedLong : [[UINT_T]] {
// CHECK:   init?(rawValue: [[UINT_T]])
// CHECK:   var rawValue: [[UINT_T]] { get }
// CHECK:   typealias RawValue = [[UINT_T]]
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }

// CHECK: enum ScopedEnumInt : Int32 {
// CHECK:   init?(rawValue: Int32)
// CHECK:   var rawValue: Int32 { get }
// CHECK:   typealias RawValue = Int32
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }

// CHECK: enum ScopedEnumNegativeElement : Int32 {
// CHECK:   init?(rawValue: Int32)
// CHECK:   var rawValue: Int32 { get }
// CHECK:   typealias RawValue = Int32
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }
