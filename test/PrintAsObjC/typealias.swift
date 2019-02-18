// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/typealias.swiftmodule %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -parse-as-library %t/typealias.swiftmodule -typecheck -emit-objc-header-path %t/typealias.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/typealias.h
// RUN: %check-in-clang %t/typealias.h

// RUN: %target-swift-frontend -typecheck %s -emit-objc-header-path %t/typealias_wmo.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/typealias_wmo.h
// RUN: %check-in-clang %t/typealias_wmo.h

// REQUIRES: objc_interop

public typealias ActuallyGeneric<T> = T

public struct GenericHelper<T> {
  public typealias TheType = T
}
public typealias PassesThroughGeneric = GenericHelper<Int>.TheType

// CHECK-LABEL: @interface X
@objc public class X {
  // CHECK: @property (nonatomic) NSInteger actuallyGeneric;
  @objc public var actuallyGeneric: ActuallyGeneric<Int> = 0
  // CHECK: @property (nonatomic) NSInteger passesThroughGeneric;
  @objc public var passesThroughGeneric: PassesThroughGeneric = 0
} // CHECK: @end