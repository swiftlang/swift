// RUN: %target-swift-ide-test -print-module -module-to-print=AnonymousWithSwiftName -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: @available(*, unavailable, message: "Not available in Swift")
// CHECK: typealias SOColorMask = UInt32

// CHECK: struct SOColorMask : OptionSet, @unchecked Sendable {
// CHECK:   init(rawValue: UInt32)
// CHECK:   let rawValue: UInt32
// CHECK:   typealias RawValue = UInt32
// CHECK:   typealias Element = SOColorMask
// CHECK:   typealias ArrayLiteralElement = SOColorMask

// CHECK:   static var red: SOColorMask { get }
// CHECK:   @available(swift, obsoleted: 3, renamed: "red")
// CHECK:   static var Red: SOColorMask { get }

// CHECK:   static var green: SOColorMask { get }
// CHECK:   @available(swift, obsoleted: 3, renamed: "green")
// CHECK:   static var Green: SOColorMask { get }

// CHECK:   static var blue: SOColorMask { get }
// CHECK:   @available(swift, obsoleted: 3, renamed: "blue")
// CHECK:   static var Blue: SOColorMask { get }

// CHECK:   static var all: SOColorMask { get }
// CHECK:   @available(swift, obsoleted: 3, renamed: "all")
// CHECK:   static var All: SOColorMask { get }
// CHECK: }

// CHECK: @available(*, unavailable, message: "Not available in Swift")
// CHECK: typealias CFColorMask = UInt32

// CHECK: struct CFColorMask : OptionSet {
// CHECK:   init(rawValue: UInt32)
// CHECK:   let rawValue: UInt32
// CHECK:   typealias RawValue = UInt32
// CHECK:   typealias Element = CFColorMask
// CHECK:   typealias ArrayLiteralElement = CFColorMask

// CHECK:   static var red: CFColorMask { get }
// CHECK:   @available(swift, obsoleted: 3, renamed: "red")
// CHECK:   static var Red: CFColorMask { get }

// CHECK:   static var green: CFColorMask { get }
// CHECK:   @available(swift, obsoleted: 3, renamed: "green")
// CHECK:   static var Green: CFColorMask { get }

// CHECK:   static var blue: CFColorMask { get }
// CHECK:   @available(swift, obsoleted: 3, renamed: "blue")
// CHECK:   static var Blue: CFColorMask { get }

// CHECK:   static var all: CFColorMask { get }
// CHECK:   @available(swift, obsoleted: 3, renamed: "all")
// CHECK:   static var All: CFColorMask { get }
// CHECK: }
