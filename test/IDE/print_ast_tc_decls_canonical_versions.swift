// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -verify %s
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -function-definitions=false -prefer-type-repr=false -print-implicit-attrs=true > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=PASS_COMMON -strict-whitespace < %t.printed.txt

@available(iOS 10.16, OSX 10.16, *)
func introduced10_16() {}
// PASS_COMMON: {{^}}@available(iOS 10.16, macOS 11.0, *){{$}}
// PASS_COMMON-NEXT: {{^}}func introduced10_16(){{$}}

@available(macOS 16.0, iOS 19.0, macCatalyst 19.0, watchOS 12.0, tvOS 19.0, visionOS 3.0, *)
func introducedInVersionsMappingTo26_0() {}
// FIXME: visionOS and macCatalyst are missing
// PASS_COMMON: {{^}}@available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, *){{$}}
// PASS_COMMON-NEXT: {{^}}func introducedInVersionsMappingTo26_0(){{$}}

@available(macOS 18.0, iOS 21.0, macCatalyst 21.0, watchOS 14.0, tvOS 21.0, visionOS 5.0, *)
func introducedInVersionsMappingTo28_0() {}
// FIXME: visionOS and macCatalyst are missing
// PASS_COMMON: {{^}}@available(macOS 28.0, iOS 28.0, watchOS 28.0, tvOS 28.0, *){{$}}
// PASS_COMMON-NEXT: {{^}}func introducedInVersionsMappingTo28_0(){{$}}
