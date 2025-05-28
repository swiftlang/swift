// RUN: %target-swift-ide-test -print-module -module-to-print=AnonymousWithSwiftName -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK-NOT: typealias CFColorMask = UInt32

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

// CHECK: func useCFColorMask(_ mask: CFColorMask) -> CFColorMask

// Test rename with "swift_name" attr:
// CHECK: struct ParentStruct

// CHECK: @available(swift, obsoleted: 3, renamed: "ParentStruct.childFn(self:)")
// CHECK: func renameCFColorMask(_ parent: ParentStruct) -> CFColorMask

// CHECK: extension ParentStruct {
// CHECK:     func childFn() -> CFColorMask
// CHECK:     @available(*, unavailable, message: "Not available in Swift")
// CHECK:     typealias NewName = UInt32
// CHECK:     struct NewName : OptionSet, @unchecked Sendable {
// CHECK:         init(rawValue: UInt32)
// CHECK:         let rawValue: UInt32
// CHECK:         typealias RawValue = UInt32
// CHECK:         typealias Element = ParentStruct.NewName
// CHECK:         typealias ArrayLiteralElement = ParentStruct.NewName
// CHECK:         static var one: ParentStruct.NewName { get }
// CHECK:         @available(swift, obsoleted: 3, renamed: "one")
// CHECK:         static var One: ParentStruct.NewName { get }
// CHECK:         static var two: ParentStruct.NewName { get }
// CHECK:         @available(swift, obsoleted: 3, renamed: "two")
// CHECK:         static var Two: ParentStruct.NewName { get }
// CHECK:     }
// CHECK: }

// CHECK: @available(swift, obsoleted: 3, renamed: "ParentStruct.NewName")
// CHECK: @available(*, unavailable, message: "Not available in Swift")
// CHECK: typealias OldName = ParentStruct.NewName

// CHECK: @available(swift, obsoleted: 3, renamed: "ParentStruct.NewName")
// CHECK: typealias OldName = ParentStruct.NewName
// CHECK: @available(*, unavailable, message: "Not available in Swift")
// CHECK: typealias GlobalNewName = UInt32
// CHECK: @available(swift, obsoleted: 3, renamed: "GlobalNewName")
// CHECK: @available(*, unavailable, message: "Not available in Swift")
// CHECK: typealias GlobalOldName = GlobalNewName
// CHECK: struct GlobalNewName : OptionSet, @unchecked Sendable {
// CHECK:     init(rawValue: UInt32)
// CHECK:     let rawValue: UInt32
// CHECK:     typealias RawValue = UInt32
// CHECK:     typealias Element = GlobalNewName
// CHECK:     typealias ArrayLiteralElement = GlobalNewName
// CHECK:     static var one: GlobalNewName { get }
// CHECK:     @available(swift, obsoleted: 3, renamed: "one")
// CHECK:     static var One: GlobalNewName { get }
// CHECK:     static var two: GlobalNewName { get }
// CHECK:     @available(swift, obsoleted: 3, renamed: "two")
// CHECK:     static var Two: GlobalNewName { get }
// CHECK: }
// CHECK: @available(swift, obsoleted: 3, renamed: "GlobalNewName")
// CHECK: typealias GlobalOldName = GlobalNewName
