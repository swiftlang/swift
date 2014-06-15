// RUN: rm -rf %t
// RUN: mkdir -p %t
// This file deliberately does not use %clang-importer-sdk for most RUN lines.
// Instead, it generates custom overlay modules itself, and uses -I %t when it
// wants to use them.

// RUN: %swift -emit-module -o %t %clang-importer-sdk -target x86_64-apple-darwin13 -module-cache-path %t/clang-module-cache %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN: %swift -emit-module -o %t -I %t %clang-importer-sdk -target x86_64-apple-darwin13 -module-cache-path %t/clang-module-cache %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=ctypes -sdk %S/../Inputs/clang-importer-sdk -I %t -target x86_64-apple-darwin13 -module-cache-path %t/clang-module-cache -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=TAG_DECLS_AND_TYPEDEFS -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=NEGATIVE -strict-whitespace < %t.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foundation -sdk %S/../Inputs/clang-importer-sdk -I %t -target x86_64-apple-darwin13 -module-cache-path %t/clang-module-cache -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=FOUNDATION -strict-whitespace < %t.printed.txt

// TAG_DECLS_AND_TYPEDEFS:      {{^}}struct FooStruct1 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: CInt{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: CDouble{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct2 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: CInt{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: CDouble{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}typealias FooStructTypedef1 = FooStruct2{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStructTypedef2 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: CInt{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: CDouble{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct3 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: CInt{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: CDouble{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct4 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: CInt{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: CDouble{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct5 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: CInt{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: CDouble{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}struct FooStruct6 {{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var x: CInt{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}  var y: CDouble{{$}}
// TAG_DECLS_AND_TYPEDEFS-NEXT: {{^}}}{{$}}

// NEGATIVE-NOT: typealias FooStructTypedef2

// FOUNDATION:      {{^}}/// Aaa.  NSArray.  Bbb.{{$}}
// FOUNDATION-NEXT: {{^}}class NSArray : NSObject {{{$}}

// FOUNDATION:      {{^}}/// Aaa.  NSRuncingMode.  Bbb.{{$}}
// FOUNDATION-NEXT: {{^}}enum NSRuncingMode : UInt {{{$}}
// FOUNDATION-NEXT: {{^}}  case Mince{{$}}
// FOUNDATION-NEXT: {{^}}  case Quince{{$}}
// FOUNDATION-NEXT: {{^}}}{{$}}

// FOUNDATION:      {{^}}/// Aaa.  NSRuncingOptions.  Bbb.{{$}}
// FOUNDATION-NEXT: {{^}}struct NSRuncingOptions : RawOptionSet {{{$}}
// FOUNDATION-NEXT: {{^}}  init(){{$}}
// FOUNDATION-NEXT: {{^}}  init(_ value: UInt){{$}}
// FOUNDATION-NEXT: {{^}}  var value: UInt{{$}}
// FOUNDATION-NEXT: {{^}}  static var EnableMince: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}  static var EnableQuince: NSRuncingOptions { get }{{$}}
// FOUNDATION-NEXT: {{^}}  static func fromMask(raw: UInt) -> NSRuncingOptions{{$}}
// FOUNDATION-NEXT: {{^}}  static func fromRaw(raw: UInt) -> NSRuncingOptions?{{$}}
// FOUNDATION-NEXT: {{^}}  func toRaw() -> UInt{{$}}
// FOUNDATION-NEXT: {{^}}  func getLogicValue() -> Bool{{$}}
// FOUNDATION-NEXT: {{^}}  static func convertFromNilLiteral() -> NSRuncingOptions{{$}}
// FOUNDATION-NEXT: {{^}}}{{$}}

