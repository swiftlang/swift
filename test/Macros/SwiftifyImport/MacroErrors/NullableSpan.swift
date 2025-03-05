// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

// XFAIL: *
// expanded form errors with "type 'Span' does not conform to protocol 'Escapable'" because Optional doesn't support ~Escapable yet 
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
func nullableSpan(_ ptr: UnsafePointer<CInt>?, _ len: CInt) {
}
