// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

@freestanding // expected-error {{expected arguments in '@freestanding' attribute}}
func dummy() {}

@_extern(lang: c) // expected-error@:10 {{unexpected argument label 'lang:' in '@_extern' attribute}}
func c_func() {}

@_extern() // expected-error@:10 {{expected option in '@_extern' attribute such as 'c'}}
func unknown_func() {}

@_extern("C") // expected-error@:10 {{expected an identifier in '@_extern' attribute}}
func unknown_func_str() {}

@implementation()  // expected-error@:17 {{expected arguments in '@implementation' attribute}}
func implementaiton_func() {}

@_silgen_name("whatever", extra)  // expected-error@:27 {{unexpected arguments in '@_silgen_name' attribute}}
func _whatever()

@available(*, unavailable, message: "foo", message: "bar") // expected-error@:44 {{duplicated argument 'message' in '@available' attribute}}
func unavailable_func() {}

@available(swift) // expected-error@:17 {{expected version number after 'swift'}}
func unavailable_swift() {}

@available(swift 99, message: "foo") // expected-error@:22 {{expected platform name}}
func unavailable_swift99() {}
