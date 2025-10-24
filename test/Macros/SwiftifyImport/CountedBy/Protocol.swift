// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -enable-experimental-feature Lifetimes -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature Lifetimes -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImportProtocol(.method(signature: "func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt)", paramInfo: [.countedBy(pointer: .param(1), count: "len")]))
protocol SimpleProtocol {
  func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt)
}

@_SwiftifyImportProtocol(.method(signature: "func foo(_ ptr: UnsafePointer<CInt>, _ len: CInt)", paramInfo: [.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1))]),
                         .method(signature: "func bar(_ len: CInt) -> UnsafePointer<CInt>", paramInfo: [.countedBy(pointer: .return, count: "len"), .nonescaping(pointer: .return), .lifetimeDependence(dependsOn: .self, pointer: .return, type: .borrow)]))
protocol SpanProtocol {
  func foo(_ ptr: UnsafePointer<CInt>, _ len: CInt)
  func bar(_ len: CInt) -> UnsafePointer<CInt>
}

@_SwiftifyImportProtocol(.method(signature: "func foo(_ ptr: UnsafePointer<CInt>, _ len: CInt)", paramInfo: [.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1))]),
                         .method(signature: "func bar(_ ptr: UnsafePointer<CInt>, _ len: CInt)", paramInfo: [.countedBy(pointer: .param(1), count: "len")]))
protocol MixedProtocol {
  /// Some doc comment
  func foo(_ ptr: UnsafePointer<CInt>, _ len: CInt)
  func bar(_ ptr: UnsafePointer<CInt>, _ len: CInt)
}

@_SwiftifyImportProtocol(.method(signature: "func foo(_ ptr: UnsafePointer<CInt>, _ len1: CInt)", paramInfo: [.countedBy(pointer: .param(1), count: "len1")]),
                         .method(signature: "func foo(bar: UnsafePointer<CInt>, _ len2: CInt)", paramInfo: [.countedBy(pointer: .param(1), count: "len2")]))
protocol OverloadedProtocol {
  func foo(_ ptr: UnsafePointer<CInt>, _ len1: CInt)
  func foo(bar: UnsafePointer<CInt>, _ len2: CInt)
  func foo()
}

//--- expansions.expected
@__swiftmacro_4test14SimpleProtocol015_SwiftifyImportC0fMe_.swift
------------------------------
extension SimpleProtocol {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
      func myFunc(_ ptr: UnsafeBufferPointer<CInt>) {
        let len = CInt(exactly: ptr.count)!
        return unsafe myFunc(ptr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_4test12SpanProtocol015_SwiftifyImportC0fMe_.swift
------------------------------
extension SpanProtocol {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
      func foo(_ ptr: Span<CInt>) {
        let len = CInt(exactly: ptr.count)!
        return unsafe ptr.withUnsafeBufferPointer { _ptrPtr in
          return unsafe foo(_ptrPtr.baseAddress!, len)
        }
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(borrow self) @_disfavoredOverload public
      func bar(_ len: CInt) -> Span<CInt> {
        return unsafe _swiftifyOverrideLifetime(Span<CInt>(_unsafeStart: unsafe bar(len), count: Int(len)), copying: ())
    }
}
------------------------------
@__swiftmacro_4test13MixedProtocol015_SwiftifyImportC0fMe_.swift
------------------------------
extension MixedProtocol {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
      /// Some doc comment
      func foo(_ ptr: Span<CInt>) {
        let len = CInt(exactly: ptr.count)!
        return unsafe ptr.withUnsafeBufferPointer { _ptrPtr in
          return unsafe foo(_ptrPtr.baseAddress!, len)
        }
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
      func bar(_ ptr: UnsafeBufferPointer<CInt>) {
        let len = CInt(exactly: ptr.count)!
        return unsafe bar(ptr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_4test18OverloadedProtocol015_SwiftifyImportC0fMe_.swift
------------------------------
extension OverloadedProtocol {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
      func foo(_ ptr: UnsafeBufferPointer<CInt>) {
        let len1 = CInt(exactly: ptr.count)!
        return unsafe foo(ptr.baseAddress!, len1)
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
      func foo(bar: UnsafeBufferPointer<CInt>) {
        let len2 = CInt(exactly: bar.count)!
        return unsafe foo(bar: bar.baseAddress!, len2)
    }
}
------------------------------
