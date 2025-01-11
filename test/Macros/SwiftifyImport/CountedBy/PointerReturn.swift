// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .return, count: "len"))
func myFunc(_ len: CInt) -> UnsafeMutablePointer<CInt> {
}

@_SwiftifyImport(.countedBy(pointer: .return, count: "len"), .nonescaping(pointer: .return))
func nonEscaping(_ len: CInt) -> UnsafePointer<CInt> {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {
// CHECK-NEXT:     return UnsafeMutableBufferPointer<CInt> (start: myFunc(len), count: Int(len))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func nonEscaping(_ len: CInt) -> UnsafeBufferPointer<CInt> {
// CHECK-NEXT:     return UnsafeBufferPointer<CInt> (start: nonEscaping(len), count: Int(len))
// CHECK-NEXT: }

