// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

// expected-error@+2{{void pointers not supported for countedBy}}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "count"))
func myFunc(_ ptr: UnsafeRawPointer, _ count: CInt) {
}

// expected-error@+2{{void pointers not supported for countedBy}}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "count"))
func myFunc(_ ptr: OpaquePointer, _ count: CInt) {
}
