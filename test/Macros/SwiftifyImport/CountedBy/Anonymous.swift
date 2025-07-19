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

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: public func myFunc(_ _myFunc_param0: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     let _myFunc_param1 = CInt(exactly: unsafe _myFunc_param0.count)!
// CHECK-NEXT:     return unsafe myFunc(_myFunc_param0.baseAddress!, _myFunc_param1)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: public func myFunc2(_ _myFunc2_param0: UnsafeBufferPointer<CInt>, _ _myFunc2_param2: CInt) {
// CHECK-NEXT:     let _myFunc2_param1 = CInt(exactly: unsafe _myFunc2_param0.count)!
// CHECK-NEXT:     return unsafe myFunc2(_myFunc2_param0.baseAddress!, _myFunc2_param1, _myFunc2_param2)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: public func myFunc3(_ _myFunc3_param0: Span<CInt>) {
// CHECK-NEXT:     let _myFunc3_param1 = CInt(exactly: _myFunc3_param0.count)!
// CHECK-NEXT:     return unsafe _myFunc3_param0.withUnsafeBufferPointer { __myFunc3_param0Ptr in
// CHECK-NEXT:       return unsafe myFunc3(__myFunc3_param0Ptr.baseAddress!, _myFunc3_param1)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(_myFunc4_param0: copy _myFunc4_param0) @_disfavoredOverload
// CHECK-NEXT: public func myFunc4(_ _myFunc4_param0: inout MutableSpan<CInt>) {
// CHECK-NEXT:     let _myFunc4_param1 = CInt(exactly: _myFunc4_param0.count)!
// CHECK-NEXT:     return unsafe _myFunc4_param0.withUnsafeMutableBufferPointer { __myFunc4_param0Ptr in
// CHECK-NEXT:       return unsafe myFunc4(__myFunc4_param0Ptr.baseAddress!, _myFunc4_param1)
// CHECK-NEXT:     }
// CHECK-NEXT: }
