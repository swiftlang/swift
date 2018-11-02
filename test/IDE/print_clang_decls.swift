// RUN: %empty-directory(%t)

// XFAIL: linux, freebsd

// This file deliberately does not use %clang-importer-sdk for most RUN lines.
// Instead, it generates custom overlay modules itself, and uses -I %t when it
// wants to use them.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=ctypes -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=TAG_DECLS_AND_TYPEDEFS -strict-whitespace < %t.printed.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE -strict-whitespace < %t.printed.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=Foundation -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=FOUNDATION -strict-whitespace < %t.printed.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=ctypes.bits -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=CTYPESBITS -strict-whitespace < %t.printed.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=nullability -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=CHECK-NULLABILITY -strict-whitespace < %t.printed.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=bridged_typedef -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=CHECK-BRIDGED-TYPEDEF -strict-whitespace < %t.printed.txt

// TAG_DECLS_AND_TYPEDEFS: {{^}}struct FooStruct1 {{{$}}
// TAG_DECLS_AND_TYPEDEFS: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS:      /*!
// TAG_DECLS_AND_TYPEDEFS-NEXT:   @keyword Foo2
// TAG_DECLS_AND_TYPEDEFS-NEXT: */
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

// TAG_DECLS_AND_TYPEDEFS: {{^}}struct FooStruct3 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS: {{^}}struct FooStruct4 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS: {{^}}struct FooStruct5 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS: {{^}}struct FooStruct6 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: Int32{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: Double{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  init(x: Int32, y: Double){{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// Skip through unavailable typedefs when importing types.
// TAG_DECLS_AND_TYPEDEFS: @available(*, unavailable, message: "use double")
// TAG_DECLS_AND_TYPEDEFS-NEXT: typealias real_t = Double
// TAG_DECLS_AND_TYPEDEFS-NEXT: func realSin(_ value: Double) -> Double

// NEGATIVE-NOT: typealias FooStructTypedef2

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSArray.  Bbb.{{$}}
// FOUNDATION-NEXT: {{^}}class NSArray : NSObject {{{$}}
// FOUNDATION-NEXT: init!(objects: UnsafePointer<AnyObject>?, count cnt: Int)
// FOUNDATION-NEXT: subscript(idx: Int) -> Any { get }

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSRuncingMode.  Bbb.{{$}}
// FOUNDATION-NEXT: {{^}}enum NSRuncingMode : UInt {{{$}}
// FOUNDATION-NEXT: {{^}}  init?(rawValue: UInt){{$}}
// FOUNDATION-NEXT: {{^}}  var rawValue: UInt { get }{{$}}
// FOUNDATION-NEXT: {{^}}  typealias RawValue = UInt
// FOUNDATION-NEXT: {{^}}  case mince{{$}}
// FOUNDATION-NEXT: {{^}}  @available(swift, obsoleted: 3, renamed: "mince"){{$}}
// FOUNDATION-NEXT: {{^}}  static var Mince: NSRuncingMode { get }{{$}}
// FOUNDATION-NEXT: {{^}}  case quince{{$}}
// FOUNDATION-NEXT: {{^}}  @available(swift, obsoleted: 3, renamed: "quince"){{$}}
// FOUNDATION-NEXT: {{^}}  static var Quince: NSRuncingMode { get }{{$}}
// FOUNDATION-NEXT: {{^}}}{{$}}

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSRuncingOptions.  Bbb.{{$}}
// FOUNDATION-NEXT: {{^}}struct NSRuncingOptions : OptionSet {{{$}}
// FOUNDATION-NEXT: {{^}}  init(rawValue: UInt){{$}}
// FOUNDATION-NEXT: {{^}}  let rawValue: UInt{{$}}
// FOUNDATION-NEXT: {{^}}  typealias RawValue = UInt
// FOUNDATION-NEXT: {{^}}  typealias Element = NSRuncingOptions
// FOUNDATION-NEXT: {{^}}  typealias ArrayLiteralElement = NSRuncingOptions
// FOUNDATION-NEXT: {{^}}  @available(*, unavailable, message: "use [] to construct an empty option set"){{$}}
// FOUNDATION-NEXT: {{^}}  static var none: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}  @available(*, unavailable, message: "use [] to construct an empty option set"){{$}}
// FOUNDATION-NEXT: {{^}}  @available(swift, obsoleted: 3, renamed: "none"){{$}}
// FOUNDATION-NEXT: {{^}}  static var None: NSRuncingOptions { get }
// FOUNDATION-NEXT: {{^}}  static var enableMince: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}  @available(swift, obsoleted: 3, renamed: "enableMince"){{$}}
// FOUNDATION-NEXT: {{^}}  static var EnableMince: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}  static var enableQuince: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}  @available(swift, obsoleted: 3, renamed: "enableQuince"){{$}}
// FOUNDATION-NEXT: {{^}}  static var EnableQuince: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}}{{$}}

// FOUNDATION-LABEL: {{^}}/// Unavailable Global Functions{{$}}
// FOUNDATION-NEXT: @available(*, unavailable, message: "Zone-based memory management is unavailable")
// FOUNDATION-NEXT: NSSetZoneName(_ zone: NSZone, _ name: String)

// FOUNDATION-LABEL: struct FictionalServerError
// FOUNDATION:         enum Code
// FOUNDATION:           case meltedDown
// FOUNDATION:         static var meltedDown: FictionalServerError.Code

// FOUNDATION-LABEL: extension NSLaundromat {
// FOUNDATION-NEXT:    struct Error
// FOUNDATION:           enum Code
// FOUNDATION:           case tooMuchSoap
// FOUNDATION:         static var tooMuchSoap: NSLaundromat.Error.Code { get }

// CTYPESBITS-NOT: FooStruct1
// CTYPESBITS: {{^}}typealias DWORD = Int32{{$}}
// CTYPESBITS-NEXT: {{^}}var MY_INT: Int32 { get }{{$}}
// CTYPESBITS-NOT: FooStruct1

// CHECK-NULLABILITY: func getId1() -> Any?
// CHECK-NULLABILITY: var global_id: AnyObject?
// CHECK-NULLABILITY: class SomeClass {
// CHECK-NULLABILITY:   class func methodA(_ obj: SomeClass?) -> Any{{$}}
// CHECK-NULLABILITY:   func methodA(_ obj: SomeClass?) -> Any{{$}}
// CHECK-NULLABILITY:   class func methodB(_ block: ((Int32, Int32) -> Int32)? = nil) -> Any{{$}}
// CHECK-NULLABILITY:   func methodB(_ block: ((Int32, Int32) -> Int32)? = nil) -> Any{{$}}
// CHECK-NULLABILITY:   func methodC() -> Any?
// CHECK-NULLABILITY:   var property: Any?
// CHECK-NULLABILITY:   func stringMethod() -> String{{$}}
// CHECK-NULLABILITY:   func optArrayMethod() -> [Any]?
// CHECK-NULLABILITY: }
// CHECK-NULLABILITY: func compare_classes(_ sc1: SomeClass, _ sc2: SomeClass, _ sc3: SomeClass!)

// CHECK-BRIDGED-TYPEDEF: typealias NSMyAmazingStringAlias = String
// CHECK-BRIDGED-TYPEDEF: func acceptNSMyAmazingStringAlias(_ param: NSMyAmazingStringAlias?)
// CHECK-BRIDGED-TYPEDEF: func acceptNSMyAmazingStringAliasArray(_ param: [NSMyAmazingStringAlias])
// CHECK-BRIDGED-TYPEDEF: func acceptIndirectedAmazingAlias(_ param: AutoreleasingUnsafeMutablePointer<NSString>?)
