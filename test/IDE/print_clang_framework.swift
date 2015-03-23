// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %target-swift-ide-test(mock-sdk: -F %S/Inputs/mock-sdk) -print-module -source-filename %s -module-to-print=Foo -function-definitions=false -print-regular-comments > %t/Foo.printed.txt
// RUN: diff -u %S/Inputs/mock-sdk/Foo.printed.txt %t/Foo.printed.txt

// RUN: %target-swift-ide-test(mock-sdk: -F %S/Inputs/mock-sdk) -print-module -source-filename %s -module-to-print=Foo -function-definitions=false -prefer-type-repr=true -module-print-submodules > %t/Foo.printed.recursive.txt
// RUN: diff -u %t/Foo.printed.recursive.txt %S/Inputs/mock-sdk/Foo.printed.recursive.txt

// RUN: %target-swift-ide-test(mock-sdk: -F %S/Inputs/mock-sdk) -print-module -source-filename %s -module-to-print=Foo.FooSub -function-definitions=false -prefer-type-repr=true > %t/Foo.FooSub.printed.txt
// RUN: diff -u %t/Foo.FooSub.printed.txt %S/Inputs/mock-sdk/Foo.FooSub.printed.txt

// RUN: %target-swift-ide-test(mock-sdk: -F %S/Inputs/mock-sdk) -print-module -source-filename %s -module-to-print=FooHelper -function-definitions=false -prefer-type-repr=true > %t/FooHelper.printed.txt
// RUN: diff -u %t/FooHelper.printed.txt %S/Inputs/mock-sdk/FooHelper.printed.txt

// RUN: %target-swift-ide-test(mock-sdk: -F %S/Inputs/mock-sdk) -print-module -source-filename %s -module-to-print=FooHelper.FooHelperSub -function-definitions=false -prefer-type-repr=true > %t/FooHelper.FooHelperSub.printed.txt
// RUN: diff -u %t/FooHelper.FooHelperSub.printed.txt %S/Inputs/mock-sdk/FooHelper.FooHelperSub.printed.txt

// RUN: %target-swift-ide-test(mock-sdk: -F %S/Inputs/mock-sdk) -print-module -source-filename %s -module-to-print=FooHelper.FooHelperExplicit -function-definitions=false -prefer-type-repr=true > %t/FooHelper.FooHelperExplicit.printed.txt
// RUN: diff -u %t/FooHelper.FooHelperExplicit.printed.txt %S/Inputs/mock-sdk/FooHelper.FooHelperExplicit.printed.txt

// RUN: %target-swift-ide-test(mock-sdk: -F %S/Inputs/mock-sdk) -print-module -source-filename %s -module-to-print=Foo -function-definitions=false -prefer-type-repr=true -annotate-print > %t/Foo.annotated.txt
// RUN: diff -u %t/Foo.annotated.txt %S/Inputs/mock-sdk/Foo.annotated.txt

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=Foundation -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=FOUNDATION -strict-whitespace < %t.printed.txt

// This test is in general platform-independent, but it happens to check
// printing of @availability attributes for OS X, and those are not printed on
// iOS.
//
// FIXME: split OS X parts into a separate test.
//
// REQUIRES: OS=macosx

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSUnavailableOptions.  Bbb.
// FOUNDATION-NEXT: {{^}}@availability(OSX, introduced=10.10){{$}}
// FOUNDATION-NEXT: {{^}}struct NSUnavailableOptions : RawOptionSetType {{{$}}

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSOptionsWithUnavailableElement.  Bbb.
// FOUNDATION-NEXT: {{^}}struct NSOptionsWithUnavailableElement : RawOptionSetType {{{$}}
// FOUNDATION-NEXT: {{^}}  init(){{$}}
// FOUNDATION-NEXT: {{^}}  init(_ rawValue: UInt){{$}}
// FOUNDATION-NEXT: {{^}}  init(rawValue: UInt){{$}}
// FOUNDATION-NEXT: {{^}}  let rawValue: UInt{{$}}
// FOUNDATION-NEXT: {{^}}  static var First: NSOptionsWithUnavailableElement { get }{{$}}
// FOUNDATION-NEXT: {{^}}  static var Second: NSOptionsWithUnavailableElement { get }{{$}}
// FOUNDATION-NEXT: {{^}}  @availability(OSX, introduced=10.10){{$}}
// FOUNDATION-NEXT: {{^}}  static var Third: NSOptionsWithUnavailableElement { get }{{$}}

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSUnavailableEnum.  Bbb.
// FOUNDATION-NEXT: {{^}}@availability(OSX, introduced=10.10){{$}}
// FOUNDATION-NEXT: {{^}}enum NSUnavailableEnum : UInt {{{$}}

// FOUNDATION-LABEL: {{^}}/// Aaa.  NSEnumWithUnavailableElement.  Bbb.
// FOUNDATION-NEXT: {{^}}enum NSEnumWithUnavailableElement : UInt {{{$}}
// FOUNDATION-NEXT: {{^}}  init?(rawValue: UInt){{$}}
// FOUNDATION-NEXT: {{^}}  var rawValue: UInt { get }{{$}}
// FOUNDATION-NEXT: {{^}}  case First{{$}}
// FOUNDATION-NEXT: {{^}}  case Second{{$}}
// FOUNDATION-NEXT: {{^}}  @availability(OSX, introduced=10.10){{$}}
// FOUNDATION-NEXT: {{^}}  case Third{{$}}
