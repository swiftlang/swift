// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir

// expected-error@+1{{multiple _SwiftifyInfos referring to parameter with index 1: .countedBy(pointer: .param(1), count: "dummy", nonescaping: false) and .countedBy(pointer: .param(1), count: "len", nonescaping: false)}}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .param(1), count: "dummy"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt, _ dummy: CInt) {
}
