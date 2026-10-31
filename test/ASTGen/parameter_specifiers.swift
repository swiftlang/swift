// Parameter specifiers written before the parameter name, and the ordering and
// repetition rules for 'sending', give the same diagnostics from both parsers.

// RUN: %target-typecheck-verify-swift -swift-version 5 \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -enable-experimental-feature SendingArgsAndResults
// RUN: %target-typecheck-verify-swift -swift-version 5 \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -enable-experimental-feature SendingArgsAndResults \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: concurrency
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_SendingArgsAndResults

func constBeforeName(_const _ x: Int) {}
// expected-warning@-1 {{'_const' before a parameter name is not allowed, place it before the parameter type instead; this is an error in the Swift 6 language mode}}

enum E {
  case beforeName(isolated Int)
  // expected-warning@-1 {{'isolated' before a parameter name is not allowed, place it before the parameter type instead; this is an error in the Swift 6 language mode}}
  // expected-error@-2 {{'isolated' may only be used on parameters}}
}

func sendingAfterConsuming(_ x: sending consuming Int) {}
// expected-error@-1 {{'sending' must be placed after specifier 'consuming'}}

func repeatedSending(_ x: sending sending Int) {}
// expected-error@-1 {{parameter may have at most one 'sending' specifier}}

func repeatedOwnership(_ x: consuming inout Int) {}
// expected-error@-1 {{parameter may have at most one of the 'inout', 'borrowing', or 'consuming' specifiers}}
