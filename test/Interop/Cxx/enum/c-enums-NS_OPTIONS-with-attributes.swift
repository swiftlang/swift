// RUN: %target-swift-ide-test -print-module -module-to-print=CenumsNSOptions -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s
// REQUIRES: objc_interop

import CenumsNSOptions

// CHECK:      typealias NSAttributedStringFormattingOptions = UInt
// CHECK-NEXT: struct __NSAttributedStringFormattingOptions : OptionSet, @unchecked Sendable {
// CHECK-NEXT:   init(rawValue: UInt)
// CHECK-NEXT:   let rawValue: UInt
// CHECK-NEXT:   typealias RawValue = UInt
// CHECK-NEXT:   typealias Element = __NSAttributedStringFormattingOptions
// CHECK-NEXT:   typealias ArrayLiteralElement = __NSAttributedStringFormattingOptions
// CHECK-NEXT:   static var insertArgumentAttributesWithoutMerging: __NSAttributedStringFormattingOptions { get }
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "insertArgumentAttributesWithoutMerging")
// CHECK-NEXT:   static var InsertArgumentAttributesWithoutMerging: __NSAttributedStringFormattingOptions { get }
// CHECK-NEXT:   static var applyReplacementIndexAttribute: __NSAttributedStringFormattingOptions { get }
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "applyReplacementIndexAttribute")
// CHECK-NEXT:   static var ApplyReplacementIndexAttribute: __NSAttributedStringFormattingOptions { get }
// CHECK-NEXT: }

// CHECK:      @available(*, unavailable, message: "Not available in Swift")
// CHECK-NEXT: typealias NSNameThatWillNotBeUsed = UInt
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "SomeCustomName")
// CHECK-NEXT: typealias NSNameThatWillNotBeUsed = SomeCustomName
// CHECK-NEXT: struct SomeCustomName : OptionSet, @unchecked Sendable {
// CHECK-NEXT:   init(rawValue: UInt)
// CHECK-NEXT:   let rawValue: UInt
// CHECK-NEXT:   typealias RawValue = UInt
// CHECK-NEXT:   typealias Element = SomeCustomName
// CHECK-NEXT:   typealias ArrayLiteralElement = SomeCustomName
// CHECK-NEXT:   static var memberOne: SomeCustomName { get }
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "memberOne")
// CHECK-NEXT:   static var MemberOne: SomeCustomName { get }
// CHECK-NEXT:   static var memberTwo: SomeCustomName { get }
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "memberTwo")
// CHECK-NEXT:   static var MemberTwo: SomeCustomName { get }
// CHECK-NEXT: }

// CHECK:      extension NSAttributedString {
// CHECK-NEXT:   init!(__options options: __NSAttributedStringFormattingOptions = [])
// CHECK-NEXT:   init!(__otherOptions options: SomeCustomName)
// CHECK-NEXT: }

