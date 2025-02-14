// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -enable-experimental-feature Span -enable-experimental-feature LifetimeDependence 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImportProtocol(.method(name: "myFunc", paramInfo: [.countedBy(pointer: .param(1), count: "len")]))
protocol SimpleProtocol {
  func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt)
}

// CHECK:      extension SimpleProtocol {
// CHECK-NEXT:   @_alwaysEmitIntoClient
// CHECK-NEXT:   func myFunc(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:       return unsafe myFunc(ptr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:   }
// CHECK-NEXT: }

@_SwiftifyImportProtocol(.method(name: "foo", paramInfo: [.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1))]),
                         .method(name: "bar", paramInfo: [.countedBy(pointer: .return, count: "len"), .nonescaping(pointer: .return), .lifetimeDependence(dependsOn: .self, pointer: .return, type: .borrow)]))
protocol SpanProtocol {
  func foo(_ ptr: UnsafePointer<CInt>, _ len: CInt)
  func bar(_ len: CInt) -> UnsafePointer<CInt>
}

// CHECK:       extension SpanProtocol {
// CHECK-NEXT:     @_alwaysEmitIntoClient
// CHECK-NEXT:       func foo(_ ptr: Span<CInt>) {
// CHECK-NEXT:         return unsafe ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:             return unsafe foo(_ptrPtr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:           }
// CHECK-NEXT:     }

// CHECK-NEXT:     @_alwaysEmitIntoClient @lifetime(borrow self) @_disfavoredOverload
// CHECK-NEXT:       func bar(_ len: CInt) -> Span<CInt> {
// CHECK-NEXT:         return unsafe Span<CInt>(_unsafeStart: unsafe bar(len), count: Int(len))
// CHECK-NEXT:     }
// CHECK-NEXT: }

@_SwiftifyImportProtocol(.method(name: "foo", paramInfo: [.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1))]),
                         .method(name: "bar", paramInfo: [.countedBy(pointer: .param(1), count: "len")]))
protocol MixedProtocol {
  func foo(_ ptr: UnsafePointer<CInt>, _ len: CInt)
  func bar(_ ptr: UnsafePointer<CInt>, _ len: CInt)
}

// CHECK:       extension MixedProtocol {
// CHECK-NEXT:     @_alwaysEmitIntoClient
// CHECK-NEXT:       func foo(_ ptr: Span<CInt>) {
// CHECK-NEXT:         return unsafe ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:             return unsafe foo(_ptrPtr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:           }
// CHECK-NEXT:     }
// CHECK-NEXT:     @_alwaysEmitIntoClient
// CHECK-NEXT:       func bar(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:         return unsafe bar(ptr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

