// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -parse-as-library -language-mode 5 -enable-library-evolution -enable-experimental-feature CalledAttribute -module-name CalledAttribute -emit-module-interface-path %t/calledonce.swiftinterface %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/calledonce.swiftinterface -module-name CalledAttribute
// RUN: %FileCheck %s --input-file %t/calledonce.swiftinterface

// REQUIRES: swift_feature_CalledAttribute

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public typealias FnType = @called(atMostOnce) () -> ()
// CHECK: #endif
public typealias FnType = @called(atMostOnce) () -> ()

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func test1(_: consuming @called(atMostOnce) () -> ())
// CHECK: #endif
public func test1(_: @called(atMostOnce) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func test2(_: consuming @autoclosure @called(atMostOnce) () -> ())
// CHECK: #endif
public func test2(_: @autoclosure @called(atMostOnce) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func test3(_: () -> @called(atMostOnce) () -> Swift::Void)
// CHECK: #endif
public func test3(_: () -> @called(atMostOnce) () -> Void) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func test4(_: consuming @escaping @called(atMostOnce) () -> ())
// CHECK: #endif
public func test4(_: @escaping @called(atMostOnce) () -> ()) {}

public struct Test: ~Copyable {
  // CHECK: #if compiler(>=5.3) && $CalledAttribute
  // CHECK: public let prop: (@called(atMostOnce) () -> Swift::Void)?
  // CHECK: #endif
  public let prop: (@called(atMostOnce) () -> Void)? = nil

  // CHECK: #if compiler(>=5.3) && $CalledAttribute
  // CHECK: public func f(_: (consuming @called(atMostOnce) () -> Swift::Void) -> Swift::Void)
  // CHECK: #endif
  public func f(_: (@called(atMostOnce) () -> Void) -> Void) {}
}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public typealias ExactlyOnceFnType = @called(exactlyOnce) () -> ()
// CHECK: #endif
public typealias ExactlyOnceFnType = @called(exactlyOnce) () -> ()

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func exactlyOnce1(_: consuming @called(exactlyOnce) () -> ())
// CHECK: #endif
public func exactlyOnce1(_: @called(exactlyOnce) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func exactlyOnce2(_: consuming @autoclosure @called(exactlyOnce) () -> ())
// CHECK: #endif
public func exactlyOnce2(_: @autoclosure @called(exactlyOnce) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func exactlyOnce3(_: () -> @called(exactlyOnce) () -> Swift::Void)
// CHECK: #endif
public func exactlyOnce3(_: () -> @called(exactlyOnce) () -> Void) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func exactlyOnce4(_: consuming @escaping @called(exactlyOnce) () -> ())
// CHECK: #endif
public func exactlyOnce4(_: @escaping @called(exactlyOnce) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func exactlyOnce5(_: (consuming @called(exactlyOnce) () -> Swift::Void) -> Swift::Void)
// CHECK: #endif
public func exactlyOnce5(_: (@called(exactlyOnce) () -> Void) -> Void) {}
