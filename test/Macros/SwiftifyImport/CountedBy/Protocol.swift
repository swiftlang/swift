// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -enable-experimental-feature Span 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImportProtocol(.method(name: "myFunc", paramInfo: [.countedBy(pointer: .param(1), count: "len")]))
protocol SimpleProtocol {
  func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt)
}

// CHECK:      extension SimpleProtocol {
// CHECK-NEXT:   @_alwaysEmitIntoClient
// CHECK-NEXT:   func myFunc(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:       return myFunc(ptr.baseAddress!, CInt(exactly: ptr.count)!)
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
// CHECK-NEXT:         return   ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:             return foo(_ptrPtr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:           }
// CHECK-NEXT:     }
// FIXME: Span return type support for countedBy
// CHECK-NEXT:     @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT:       func bar(_ len: CInt) -> UnsafeBufferPointer<CInt> {
// CHECK-NEXT:         return UnsafeBufferPointer<CInt>(start: bar(len), count: Int(len))
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
// CHECK-NEXT:         return   ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:             return foo(_ptrPtr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:           }
// CHECK-NEXT:     }
// CHECK-NEXT:     @_alwaysEmitIntoClient
// CHECK-NEXT:       func bar(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:         return bar(ptr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

