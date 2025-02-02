// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

// expected-error@+2{{SizedBy only supported for raw pointers}}
@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
func myFunc(_ ptr: UnsafePointer<Int>, _ size: CInt) {
}

// expected-error@+2{{raw pointers only supported for SizedBy}}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "count"))
func myFunc(_ ptr: UnsafeRawPointer, _ count: CInt) {
}

// expected-error@+2{{raw pointers only supported for SizedBy}}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "count"))
func myFunc(_ ptr: OpaquePointer, _ count: CInt) {
}

