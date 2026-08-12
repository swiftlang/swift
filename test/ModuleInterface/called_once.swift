// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -parse-as-library -language-mode 5 -enable-library-evolution -enable-experimental-feature CalledAttribute -module-name CalledAttribute -emit-module-interface-path %t/calledonce.swiftinterface %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/calledonce.swiftinterface -module-name CalledAttribute
// RUN: %FileCheck %s --input-file %t/calledonce.swiftinterface

// REQUIRES: swift_feature_CalledAttribute

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public typealias FnType = @called(once) () -> ()
// CHECK: #endif
public typealias FnType = @called(once) () -> ()

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func test1(_: consuming @called(once) () -> ())
// CHECK: #endif
public func test1(_: @called(once) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func test2(_: consuming @autoclosure @called(once) () -> ())
// CHECK: #endif
public func test2(_: @autoclosure @called(once) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func test3(_: () -> @called(once) () -> Swift::Void)
// CHECK: #endif
public func test3(_: () -> @called(once) () -> Void) {}

// CHECK: #if compiler(>=5.3) && $CalledAttribute
// CHECK: public func test4(_: consuming @escaping @called(once) () -> ())
// CHECK: #endif
public func test4(_: @escaping @called(once) () -> ()) {}

public struct Test: ~Copyable {
  // CHECK: #if compiler(>=5.3) && $CalledAttribute
  // CHECK: public let prop: (@called(once) () -> Swift::Void)?
  // CHECK: #endif
  public let prop: (@called(once) () -> Void)? = nil

  // CHECK: #if compiler(>=5.3) && $CalledAttribute
  // CHECK: public func f(_: (consuming @called(once) () -> Swift::Void) -> Swift::Void)
  // CHECK: #endif
  public func f(_: (@called(once) () -> Void) -> Void) {}
}

