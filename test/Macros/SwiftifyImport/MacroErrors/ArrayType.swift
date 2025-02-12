// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
// expected-error@+1{{expected pointer type, got [CInt] with kind arrayType}}
func myFunc(_ ptr: [CInt], _ len: String) {
}
