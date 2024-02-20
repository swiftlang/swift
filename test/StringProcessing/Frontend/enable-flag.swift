// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-bare-slash-regex -enable-experimental-string-processing
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-string-processing -enable-bare-slash-regex
// RUN: %target-typecheck-verify-swift -disable-availability-checking -disable-experimental-string-processing -enable-experimental-string-processing -enable-bare-slash-regex

// REQUIRES: swift_swift_parser

prefix operator /

_ = /x/
_ = #/x/#

@available(SwiftStdlib 5.7, *)
func foo(_ x: Regex<Substring>) {}
