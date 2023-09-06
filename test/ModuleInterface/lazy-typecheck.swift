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
// CHECK:       #if compiler(>=5.3) && $RethrowsProtocol
// CHECK:       @rethrows public protocol PublicRethrowsProto {
// CHECK:         func req() throws -> Swift.Int
// CHECK:       }
// CHECK:       #endif
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
// CHECK:       @_inheritsConvenienceInitializers public class PublicDerivedClass : lazy_typecheck.PublicClass {
// CHECK:         override public init(x: Swift.Int)
// CHECK:         deinit
// CHECK:       }
// CHECK:       public struct PublicStructConformingToPublicProto : lazy_typecheck.PublicProto {
// CHECK:         public init()
// CHECK:         public func req() -> Swift.Int
// CHECK:       }
// CHECK:       public struct PublicStructIndirectlyConformingToPublicProto {
// CHECK:         public init()
// CHECK:         public func req() -> Swift.Int
// CHECK:       }
// CHECK:       public class PublicClassConformingToPublicProto : lazy_typecheck.PublicProto {
// CHECK:         public init()
// CHECK:         public func req() -> Swift.Int
// CHECK:         deinit
// CHECK:       }
// CHECK:       @_inheritsConvenienceInitializers public class PublicClassInheritingConformanceToPublicProto : lazy_typecheck.PublicClassConformingToPublicProto {
// CHECK:         override public init()
// CHECK:         deinit
// CHECK:       }
// CHECK:       extension Swift.String : lazy_typecheck.PublicProto {
// CHECK:         public func req() -> Swift.Int
// CHECK:       }
// CHECK:       #if compiler(>=5.3) && $RethrowsProtocol
// CHECK:       extension Swift.Int : lazy_typecheck.PublicRethrowsProto {
// CHECK:         public func req() throws -> Swift.Int
// CHECK:       }
// CHECK:       #endif
// CHECK:       extension lazy_typecheck.PublicStructIndirectlyConformingToPublicProto : lazy_typecheck.PublicProto {}
