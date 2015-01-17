// RUN: rm -rf %t
// RUN: mkdir -p %t
// This file deliberately does not use %clang-importer-sdk for most RUN lines.
// Instead, it generates custom overlay modules itself, and uses -I %t when it
// wants to use them.

// RUN: %swift -emit-module -o %t %clang-importer-sdk -target x86_64-apple-macosx10.9 %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN: %swift -emit-module -o %t %clang-importer-sdk -target x86_64-apple-macosx10.9 %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN: %swift -emit-module -o %t -I %t %clang-importer-sdk -target x86_64-apple-macosx10.9 %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %swift -emit-module -o %t -I %t %clang-importer-sdk -target x86_64-apple-macosx10.9 %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift
// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=ctypes -sdk %S/../Inputs/clang-importer-sdk -I %t -target x86_64-apple-macosx10.9 -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=TAG_DECLS_AND_TYPEDEFS -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=NEGATIVE -strict-whitespace < %t.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foundation -sdk %S/../Inputs/clang-importer-sdk -I %t -target x86_64-apple-macosx10.9 -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=FOUNDATION -strict-whitespace < %t.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=AppKit -sdk %S/../Inputs/clang-importer-sdk -I %t -target x86_64-apple-macosx10.9 -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=APPKIT -strict-whitespace < %t.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=ctypes.bits -sdk %S/../Inputs/clang-importer-sdk -I %t -target x86_64-apple-macosx10.9 -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=CTYPESBITS -strict-whitespace < %t.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=nullability -sdk %S/../Inputs/clang-importer-sdk -I %t -target x86_64-apple-macosx10.9 -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=CHECK-NULLABILITY -strict-whitespace < %t.printed.txt

// TAG_DECLS_AND_TYPEDEFS:      {{^}}struct FooStruct1 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct2 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}typealias FooStructTypedef1 = FooStruct2{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStructTypedef2 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct3 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct4 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct5 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct6 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// NEGATIVE-NOT: typealias FooStructTypedef2

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSArray.  Bbb.{{$}}
// FOUNDATION-NEXT: {{^}}class NSArray : NSObject {{{$}}
// FOUNDATION-NEXT  func objectAtIndex(index: Int) -> AnyObject!

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSRuncingMode.  Bbb.{{$}}
// FOUNDATION-NEXT: {{^}}enum NSRuncingMode : UInt {{{$}}
// FOUNDATION-NEXT: {{^}}  init?(rawValue: UInt){{$}}
// FOUNDATION-NEXT: {{^}}  var rawValue: UInt { get }{{$}}
// FOUNDATION-NEXT: {{^}}  case Mince{{$}}
// FOUNDATION-NEXT: {{^}}  case Quince{{$}}
// FOUNDATION-NEXT: {{^}}}{{$}}

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSRuncingOptions.  Bbb.{{$}}
// FOUNDATION-NEXT: {{^}}struct NSRuncingOptions : RawOptionSetType {{{$}}
// FOUNDATION-NEXT: {{^}}  init(){{$}}
// FOUNDATION-NEXT: {{^}}  init(_ rawValue: UInt){{$}}
// FOUNDATION-NEXT: {{^}}  init(rawValue: UInt){{$}}
// FOUNDATION-NEXT: {{^}}  let rawValue: UInt{{$}}
// FOUNDATION-NEXT: {{^}}  static var EnableMince: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}  static var EnableQuince: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}  static var allZeros: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}  init(nilLiteral _: ()){{$}}
// FOUNDATION-NEXT: {{^}}}{{$}}

// FOUNDATION-LABEL: {{^}}/// Unavailable Global Functions{{$}}
// FOUNDATION-NEXT: @availability(*, unavailable)
// FOUNDATION-NEXT: NSSetZoneName(zone: NSZone, name: String)

// APPKIT-LABEL: {{^}}extension NSString {{{$}}

// APPKIT-LABEL: {{^}}class NSView : NSObject, NSCoding, NSAccessibility, NSObjectProtocol {{{$}}
// APPKIT-NEXT: init?(coder aDecoder: NSCoder)
// APPKIT-NEXT: func isDescendantOf(aView: NSView) -> Bool
// APPKIT-NEXT: func ancestorSharedWithView(aView: NSView) -> NSView?
// APPKIT-NEXT: func addSubview(aView: NSView)
// APPKIT-NEXT: func addSubview(aView: NSView, positioned place: UInt32, relativeTo otherView: NSView?)
// APPKIT-NEXT: unowned(unsafe) var superview: @sil_unmanaged NSView? { get }
// APPKIT-NEXT: var layer: CALayer?
// APPKIT-NEXT: var trackingAreas: [AnyObject] { get }
// APPKIT-NEXT: var subviews: [AnyObject]
// APPKIT-LABEL:      extension NSView {
// APPKIT-NEXT:   unowned(unsafe) var nextKeyView: @sil_unmanaged NSView?

// APPKIT-LABEL: {{^}}class NSMenuItem : NSObject, NSCopying, NSCoding {
// APPKIT-NEXT: unowned(unsafe) var menu: @sil_unmanaged NSMenu?
// APPKIT-NEXT: var title: String
// APPKIT-NEXT: @NSCopying var attributedTitle: NSAttributedString?
// APPKIT-NEXT: weak var target: @sil_weak AnyObject
// APPKIT-NEXT: var action: Selector
// APPKIT: {{^}}}{{$}}
// APPKIT: let NSViewFrameDidChangeNotification: String
// APPKIT: let NSViewFocusDidChangeNotification: String

// CTYPESBITS-NOT: FooStruct1
// CTYPESBITS: {{^}}typealias DWORD = Int32{{$}}
// CTYPESBITS-NEXT: {{^}}var MY_INT: Int32 { get }{{$}}
// CTYPESBITS-NOT: FooStruct1

// CHECK-NULLABILITY: typealias nonnull_id = AnyObject
// CHECK-NULLABILITY: func getId1() -> AnyObject?
// CHECK-NULLABILITY: func getId2() -> nonnull_id?
// CHECK-NULLABILITY: var global_id: AnyObject?
// CHECK-NULLABILITY: class SomeClass {
// CHECK-NULLABILITY:   class func methodA(obj: SomeClass?) -> AnyObject{{$}}
// CHECK-NULLABILITY:   func methodA(obj: SomeClass?) -> AnyObject{{$}}
// CHECK-NULLABILITY:   class func methodB(block: ((Int32, Int32) -> Int32)?) -> nonnull_id{{$}}
// CHECK-NULLABILITY:   func methodB(block: ((Int32, Int32) -> Int32)?) -> nonnull_id{{$}}
// CHECK-NULLABILITY:   func methodC() -> nonnull_id?
// CHECK-NULLABILITY:   var property: nonnull_id?
// CHECK-NULLABILITY:   func stringMethod() -> String{{$}}
// CHECK-NULLABILITY:   func optArrayMethod() -> [AnyObject]?
// CHECK-NULLABILITY: }
