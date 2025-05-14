// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func myFunc(_: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func myFunc2(_ p: UnsafePointer<CInt>, _ len: CInt, _: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
public func myFunc3(_: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
public func myFunc4(_: UnsafeMutablePointer<CInt>, _ len: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: public func myFunc(_ _param0: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     return unsafe myFunc(_param0.baseAddress!, CInt(exactly: _param0.count)!)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: public func myFunc2(_ p: UnsafeBufferPointer<CInt>, _ _param2: CInt) {
// CHECK-NEXT:     return unsafe myFunc2(p.baseAddress!, CInt(exactly: p.count)!, _param2)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: public func myFunc3(_ _param0: Span<CInt>) {
// CHECK-NEXT:     return unsafe _param0.withUnsafeBufferPointer { __param0Ptr in
// CHECK-NEXT:         return unsafe myFunc3(__param0Ptr.baseAddress!, CInt(exactly: __param0Ptr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(_param0: copy _param0)
// CHECK-NEXT: public func myFunc4(_ _param0: inout MutableSpan<CInt>) {
// CHECK-NEXT:     return unsafe _param0.withUnsafeMutableBufferPointer { __param0Ptr in
// CHECK-NEXT:         return unsafe myFunc4(__param0Ptr.baseAddress!, CInt(exactly: __param0Ptr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }