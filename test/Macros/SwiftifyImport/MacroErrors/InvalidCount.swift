// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

// expected-error@+1{{no parameter with name 'foo' in '_ ptr: UnsafePointer<CInt>, _ len: CInt'}}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "foo"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt) {
}
