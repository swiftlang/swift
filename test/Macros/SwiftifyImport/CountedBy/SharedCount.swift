// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .param(2), count: "len"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .param(2), count: "len * size"))
func myFunc2(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ len: CInt, _ size: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .param(2), count: "size"), .countedBy(pointer: .param(3), count: "len * size"))
func myFunc3(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ ptr3: UnsafePointer<CInt>, _ len: CInt, _ size: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(3), count: "len"), .countedBy(pointer: .param(2), count: "size"), .countedBy(pointer: .param(1), count: "len * size"))
func myFunc4(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ ptr3: UnsafePointer<CInt>, _ len: CInt, _ size: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len * size"), .countedBy(pointer: .param(3), count: "len"), .countedBy(pointer: .param(2), count: "size"))
func myFunc5(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ ptr3: UnsafePointer<CInt>, _ len: CInt, _ size: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     let len = CInt(exactly: unsafe ptr.count)!
// CHECK-NEXT:     if unsafe ptr2.count != len {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc: expected \(len) but got \(unsafe ptr2.count)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe myFunc(ptr.baseAddress!, ptr2.baseAddress!, len)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc2(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>, _ size: CInt) {
// CHECK-NEXT:     let len = CInt(exactly: unsafe ptr.count)!
// CHECK-NEXT:     let _ptr2Count = unsafe ptr2.count
// CHECK-NEXT:     if _ptr2Count != len * size {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc2: expected \(len * size) but got \(_ptr2Count)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe myFunc2(ptr.baseAddress!, ptr2.baseAddress!, len, size)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc3(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>, _ ptr3: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     let len = CInt(exactly: unsafe ptr.count)!
// CHECK-NEXT:     let size = CInt(exactly: unsafe ptr2.count)!
// CHECK-NEXT:     let _ptr3Count = unsafe ptr3.count
// CHECK-NEXT:     if _ptr3Count != len * size {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc3: expected \(len * size) but got \(_ptr3Count)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe myFunc3(ptr.baseAddress!, ptr2.baseAddress!, ptr3.baseAddress!, len, size)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc4(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>, _ ptr3: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     let size = CInt(exactly: unsafe ptr2.count)!
// CHECK-NEXT:     let len = CInt(exactly: unsafe ptr3.count)!
// CHECK-NEXT:     let _ptrCount = unsafe ptr.count
// CHECK-NEXT:     if _ptrCount != len * size {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc4: expected \(len * size) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe myFunc4(ptr.baseAddress!, ptr2.baseAddress!, ptr3.baseAddress!, len, size)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc5(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>, _ ptr3: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     let size = CInt(exactly: unsafe ptr2.count)!
// CHECK-NEXT:     let len = CInt(exactly: unsafe ptr3.count)!
// CHECK-NEXT:     let _ptrCount = unsafe ptr.count
// CHECK-NEXT:     if _ptrCount != len * size {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc5: expected \(len * size) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe myFunc5(ptr.baseAddress!, ptr2.baseAddress!, ptr3.baseAddress!, len, size)
// CHECK-NEXT: }
