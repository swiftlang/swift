// REQUIRES: swift_swift_parser, asserts
// REQUIRES: OS=macosx

// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name MacrosTest -target %target-cpu-apple-macos50 -plugin-path %swift-plugin-dir

@attached(attribute)
public macro fixedAttributes(_ attrs: String...) = #externalMacro(module: "SwiftMacros", type: "FixedAttributesMacro")

@fixedAttributes("@available(macOS 99, *)") func fn1() {}

fn1()
// expected-error@-1 {{'fn1()' is only available in macOS 99 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

@attached(attribute)
macro futureDecl() = #fixedAttributes("@available(macOS 99, *)")

@futureDecl func fn2() {}

fn2()
// expected-error@-1 {{'fn2()' is only available in macOS 99 or newer}}
// expected-note@-2 {{add 'if #available' version check}}
