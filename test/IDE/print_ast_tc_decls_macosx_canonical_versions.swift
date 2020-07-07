// RUN: %empty-directory(%t)
//
//
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -swift-version 4 -typecheck -verify %s -F %S/Inputs/mock-sdk -enable-objc-interop -disable-objc-attr-requires-foundation-module
//
// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -swift-version 4 -skip-deinit=false -print-ast-typechecked -source-filename %s -F %S/Inputs/mock-sdk -function-definitions=false -prefer-type-repr=false -print-implicit-attrs=true -enable-objc-interop -disable-objc-attr-requires-foundation-module > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=PASS_COMMON -strict-whitespace < %t.printed.txt


// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// REQUIRES: objc_interop

@available(iOS 10.16, OSX 10.16, *)
func introduced10_16() {}
// PASS_COMMON: {{^}}@available(iOS 10.16, OSX 11.0, *){{$}}
// PASS_COMMON-NEXT: {{^}}func introduced10_16(){{$}}
