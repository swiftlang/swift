// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -enable-bare-slash-regex -enable-experimental-string-processing
// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -enable-experimental-string-processing -enable-bare-slash-regex
// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -disable-experimental-string-processing -enable-experimental-string-processing -enable-bare-slash-regex

// REQUIRES: swift_swift_parser

prefix operator /

_ = /x/
_ = #/x/#

@available(SwiftStdlib 5.7, *)
func foo(_ x: Regex<Substring>) {}
