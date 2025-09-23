//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: String) {
}
// expected-note@-3 2{{in expansion of macro '_SwiftifyImport' on global function 'myFunc' here}}

// expected-error@@__swiftmacro_4main6myFunc15_SwiftifyImportfMp_.swift:4:15{{no exact matches in call to initializer}}
// expected-error@@__swiftmacro_4main6myFunc15_SwiftifyImportfMp_.swift:4:48{{cannot force unwrap value of non-optional type 'String'}}

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/test.swift -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -verify 2>&1 | %FileCheck %s --match-full-lines --strict-whitespace

// CHECK:@__swiftmacro_4main6myFunc15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT:func myFunc(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:    let len = String(exactly: unsafe ptr.count)!
// CHECK-NEXT:    return unsafe myFunc(ptr.baseAddress!, len)
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
