// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

let countString = "len"
// expected-error@+1{{expected string literal for 'count' parameter, got countString}}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: countString))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: String) {
}
