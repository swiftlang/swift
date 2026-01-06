// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

// expected-error@+1{{pointer index out of bounds}}
@_SwiftifyImport(.countedBy(pointer: .param(3), count: "len"))
// expected-note@+1{{function myFunc has parameter indices 1..2}}
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt) {
}
// expected-error@+1{{pointer index out of bounds}}
@_SwiftifyImport(.countedBy(pointer: .param(0), count: "len"))
// expected-note@+1{{function myFunc2 has parameter indices 1..2}}
func myFunc2(_ ptr: UnsafePointer<CInt>, _ len: CInt) {
}
// expected-error@+1{{pointer index out of bounds}}
@_SwiftifyImport(.countedBy(pointer: .param(0), count: "1"))
// expected-note@+1{{function myFunc3 has no parameters}}
func myFunc3() {
}
