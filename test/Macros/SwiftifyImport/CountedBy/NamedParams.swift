// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func ptrNamed(ptr: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func ptrNamedOther(buf ptr: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func lenNamed(_ ptr: UnsafePointer<CInt>, len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func lenNamedOther(_ ptr: UnsafePointer<CInt>, count len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func allNamed(ptr: UnsafePointer<CInt>, len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func allNamedOther(buf ptr: UnsafePointer<CInt>, count len: CInt) {
}

// CHECK: @_alwaysEmitIntoClient
// CHECK-NEXT: func ptrNamed(ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     return ptrNamed(ptr: ptr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

// CHECK: @_alwaysEmitIntoClient
// CHECK-NEXT: func ptrNamedOther(buf ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     return ptrNamedOther(buf: ptr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

// CHECK: @_alwaysEmitIntoClient
// CHECK-NEXT: func lenNamed(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     return lenNamed(ptr.baseAddress!, len: CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

// CHECK: @_alwaysEmitIntoClient
// CHECK-NEXT: func lenNamedOther(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     return lenNamedOther(ptr.baseAddress!, count: CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

// CHECK: @_alwaysEmitIntoClient
// CHECK-NEXT: func allNamed(ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     return allNamed(ptr: ptr.baseAddress!, len: CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

// CHECK: @_alwaysEmitIntoClient
// CHECK-NEXT: func allNamedOther(buf ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     return allNamedOther(buf: ptr.baseAddress!, count: CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

