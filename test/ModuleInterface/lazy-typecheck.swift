// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -typecheck -emit-module-interface-path %t/lazy_typecheck.swiftinterface -enable-library-evolution -parse-as-library -package-name Package -experimental-lazy-typecheck -experimental-skip-all-function-bodies -experimental-serialize-external-decls-only
// RUN: %FileCheck %s < %t/lazy_typecheck.swiftinterface

// RUN: rm -rf %t/*.swiftmodule
// RUN: %target-swift-frontend -package-name Package -typecheck %S/../Inputs/lazy_typecheck_client.swift -I %t

// CHECK: import Swift

// CHECK:       public func publicFunc() -> Swift.Int
// CHECK:       publicFuncWithDefaultArg(_ x: Swift.Int = 1) -> Swift.Int
// CHECK:       @inlinable internal func inlinableFunc() -> Swift.Int {
// CHECK-NEXT:    return 1
// CHECK-NEXT:  }
// CHECK:       public func constrainedGenericPublicFunction<T>(_ t: T) where T : lazy_typecheck.PublicProto
// CHECK:       @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
// CHECK-NEXT:  public func publicFuncWithOpaqueReturnType() -> some lazy_typecheck.PublicProto

// CHECK:       @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
// CHECK-NEXT:  @_alwaysEmitIntoClient public func publicAEICFuncWithOpaqueReturnType() -> some Any {
// CHECK-NEXT:    if #available(macOS 20, *) {
// CHECK-NEXT:      return 3
// CHECK-NEXT:    } else {
// CHECK-NEXT:      return "hi"
// CHECK-NEXT:    }
// CHECK-NEXT:  }

// CHECK:       public protocol PublicProto {
// CHECK:         func req() -> Swift.Int
// CHECK:       }

// CHECK:       public struct PublicStruct {
// CHECK:         public init(x: Swift.Int)
// CHECK:         public func publicMethod() -> Swift.Int
// CHECK:         public static func publicStaticMethod()
// CHECK:       }

// CHECK:       public class PublicClass {
// CHECK:         public init(x: Swift.Int)
// CHECK:         public func publicMethod() -> Swift.Int
// CHECK:         public class func publicClassMethod()
// CHECK:         deinit
// CHECK:       }
