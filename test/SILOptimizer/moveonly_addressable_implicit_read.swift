// RUN: %target-swift-frontend -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableTypes -emit-sil %s | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

public struct Butt: ~Copyable, ~Escapable {
  // The compiler will synthesize an implicit `_read` coroutine for external
  // users of this property. This test ensures that the generated code is
  // properly move-checked.
  // CHECK-LABEL: sil{{.*}} @${{.*}}4ButtV3foo{{.*}}vr
  // CHECK:       bb0([[SELF:%.*]] : $Butt):
  // CHECK:         [[FOO:%.*]] = struct_extract [[SELF]]
  // CHECK:         yield [[FOO]]
  public let foo: Tubb
}

@_addressableForDependencies
public struct Tubb: ~Copyable, ~Escapable {
  var x: AnyObject
  @_lifetime(immortal) init() { fatalError() }
}
