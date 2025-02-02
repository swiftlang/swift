// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

let countedBy = _SwiftifyInfo.countedBy(pointer: .param(1), count: "len")
// expected-error@+1{{expected _SwiftifyInfo enum literal as argument, got 'countedBy'}}
@_SwiftifyImport(countedBy)
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: String) {
}
