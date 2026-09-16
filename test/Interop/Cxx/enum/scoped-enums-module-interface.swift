// RUN: %target-swift-ide-test -print-module -module-to-print=ScopedEnums -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: enum ScopedEnumDefined : CInt {
// CHECK:   init?(rawValue: CInt)
// CHECK:   var rawValue: CInt { get }
// CHECK:   typealias RawValue = CInt
// CHECK:   case x
// CHECK:   case y
// CHECK: }

// CHECK: enum ScopedEnumBasic : CInt {
// CHECK:   init?(rawValue: CInt)
// CHECK:   var rawValue: CInt { get }
// CHECK:   typealias RawValue = CInt
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

// CHECK: enum ScopedEnumUnsignedDefined : CUnsignedInt {
// CHECK:   init?(rawValue: CUnsignedInt)
// CHECK:   var rawValue: CUnsignedInt { get }
// CHECK:   typealias RawValue = CUnsignedInt
// CHECK:   case x
// CHECK:   case y
// CHECK: }

// CHECK: enum ScopedEnumUnsignedLongDefined : CUnsignedLong {
// CHECK:   init?(rawValue: CUnsignedLong)
// CHECK:   var rawValue: CUnsignedLong { get }
// CHECK:   typealias RawValue = CUnsignedLong
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

// CHECK: enum ScopedEnumUnsigned : CUnsignedInt {
// CHECK:   init?(rawValue: CUnsignedInt)
// CHECK:   var rawValue: CUnsignedInt { get }
// CHECK:   typealias RawValue = CUnsignedInt
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }

// CHECK: enum ScopedEnumUnsignedLong : CUnsignedLong {
// CHECK:   init?(rawValue: CUnsignedLong)
// CHECK:   var rawValue: CUnsignedLong { get }
// CHECK:   typealias RawValue = CUnsignedLong
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }

// CHECK: enum ScopedEnumInt : CInt {
// CHECK:   init?(rawValue: CInt)
// CHECK:   var rawValue: CInt { get }
// CHECK:   typealias RawValue = CInt
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }

// CHECK: enum ScopedEnumNegativeElement : CInt {
// CHECK:   init?(rawValue: CInt)
// CHECK:   var rawValue: CInt { get }
// CHECK:   typealias RawValue = CInt
// CHECK:   case x
// CHECK:   case y
// CHECK:   case z
// CHECK: }

// CHECK: enum ScopedEnumChar32 : UInt32 {
// CHECK:   init?(rawValue: UInt32)
// CHECK:   var rawValue: UInt32 { get }
// CHECK:   typealias RawValue = UInt32
// CHECK:   case x
// CHECK:   case y
// CHECK: }

// CHECK: enum ScopedEnumChar16 : CChar16 {
// CHECK:   init?(rawValue: CChar16)
// CHECK:   var rawValue: CChar16 { get }
// CHECK:   typealias RawValue = CChar16
// CHECK:   case a
// CHECK:   case b
// CHECK: }
