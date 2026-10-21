// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -parse-as-library -language-mode 5 -enable-library-evolution -enable-experimental-feature OnceFunctions -module-name OnceFunctions -emit-module-interface-path %t/calledonce.swiftinterface %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/calledonce.swiftinterface -module-name OnceFunctions
// RUN: %FileCheck %s --input-file %t/calledonce.swiftinterface

// REQUIRES: swift_feature_OnceFunctions

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public typealias FnType = @callable(once) () -> ()
// CHECK: #endif
public typealias FnType = @callable(once) () -> ()

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func test1(_: consuming @callable(once) () -> ())
// CHECK: #endif
public func test1(_: @callable(once) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func test2(_: consuming @autoclosure @callable(once) () -> ())
// CHECK: #endif
public func test2(_: @autoclosure @callable(once) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func test3(_: () -> @callable(once) () -> Swift::Void)
// CHECK: #endif
public func test3(_: () -> @callable(once) () -> Void) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func test4(_: consuming @escaping @callable(once) () -> ())
// CHECK: #endif
public func test4(_: @escaping @callable(once) () -> ()) {}

public struct Test: ~Copyable {
  // CHECK: #if compiler(>=5.3) && $OnceFunctions
  // CHECK: public let prop: (@callable(once) () -> Swift::Void)?
  // CHECK: #endif
  public let prop: (@callable(once) () -> Void)? = nil

  // CHECK: #if compiler(>=5.3) && $OnceFunctions
  // CHECK: public func f(_: (consuming @callable(once) () -> Swift::Void) -> Swift::Void)
  // CHECK: #endif
  public func f(_: (@callable(once) () -> Void) -> Void) {}
}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public typealias ExactlyOnceFnType = @once () -> ()
// CHECK: #endif
public typealias ExactlyOnceFnType = @once () -> ()

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce1(_: consuming @once () -> ())
// CHECK: #endif
public func exactlyOnce1(_: @once () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce2(_: consuming @autoclosure @once () -> ())
// CHECK: #endif
public func exactlyOnce2(_: @autoclosure @once () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce3(_: () -> @once () -> Swift::Void)
// CHECK: #endif
public func exactlyOnce3(_: () -> @once () -> Void) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce4(_: consuming @escaping @once () -> ())
// CHECK: #endif
public func exactlyOnce4(_: @escaping @once () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce5(_: (consuming @once () -> Swift::Void) -> Swift::Void)
// CHECK: #endif
public func exactlyOnce5(_: (@once () -> Void) -> Void) {}
