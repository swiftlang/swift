// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "size * count"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ size: CInt, _ count: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: UnsafeBufferPointer<CInt>, _ size: CInt, _ count: CInt) {
// CHECK-NEXT:     let _ptrCount = unsafe ptr.count
// CHECK-NEXT:     if _ptrCount != size * count {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc: expected \(size * count) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe myFunc(ptr.baseAddress!, size, count)
// CHECK-NEXT: }
