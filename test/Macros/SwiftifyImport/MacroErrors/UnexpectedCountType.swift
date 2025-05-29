// REQUIRES: swift_swift_parser

// XFAIL: *

// RUN: not %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s
// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: String) {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     myFunc(ptr.baseAddress!, String(exactly: ptr.count)!)
// CHECK-NEXT: }

// expected-error@_SwiftifyImport:2{{no exact matches in call to initializer}}
