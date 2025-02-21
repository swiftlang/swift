// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

// expected-error@+2{{expected Unsafe[Mutable][Raw]Pointer type for type CInt - first type token is 'CInt'}}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func myFunc(_ ptr: CInt, _ len: CInt) {
}
// expected-error@+2{{expected Unsafe[Mutable][Raw]Pointer type for type UnsafeBufferPointer<CInt> - first type token is 'UnsafeBufferPointer'}}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func myFunc2(_ ptr: UnsafeBufferPointer<CInt>, _ len: CInt) {
}
