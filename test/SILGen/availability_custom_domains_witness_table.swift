// DEFINE: %{args} = \
// DEFINE:   -module-name main \
// DEFINE:   -enable-experimental-feature CustomAvailability \
// DEFINE:   -define-enabled-availability-domain EnabledDomain \
// DEFINE:   -define-dynamic-availability-domain DynamicDomain \
// DEFINE:   -define-disabled-availability-domain DisabledDomain

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-silgen %s -verify %{args} \
// RUN:   | %FileCheck %s --check-prefixes=CHECK

// RUN: %target-swift-frontend -emit-silgen %s -verify %{args} -O \
// RUN:   | %FileCheck %s --check-prefixes=CHECK

// RUN: %target-swift-frontend -emit-ir %s -verify %{args} -O

// REQUIRES: swift_feature_CustomAvailability

public protocol P {
  init(alwaysAvailable: Int)

  @available(EnabledDomain)
  init(enabledDomain: Int)

  @available(DisabledDomain)
  init(disabledDomain: Int)

  @available(DynamicDomain)
  init(dynamicDomain: Int)

  func alwaysAvailableFunc()

  @available(EnabledDomain)
  func enabledDomainFunc()

  @available(DisabledDomain)
  func disabledDomainFunc()

  @available(DynamicDomain)
  func dynamicDomainFunc()

  var alwaysAvailableVar: Int { get }

  @available(EnabledDomain)
  var enabledDomainVar: Int { get }

  @available(DisabledDomain)
  var disabledDomainVar: Int { get }

  @available(DynamicDomain)
  var dynamicDomainVar: Int { get }
}

public struct S: P {
  // CHECK: sil {{.*}} @$s4main1SVAA1PA2aDP15alwaysAvailablexSi_tcfCTW : $@convention(witness_method: P) (Int, @thick S.Type) -> @out S
  public init(alwaysAvailable: Int) { }

  // CHECK: sil {{.*}} @$s4main1SVAA1PA2aDP13enabledDomainxSi_tcfCTW : $@convention(witness_method: P) (Int, @thick S.Type) -> @out S
  @available(EnabledDomain)
  public init(enabledDomain: Int) { }

  // CHECK-NOT: @$s4main1SVAA1PA2aDP14disabledDomainxSi_tcfCTW
  public init(disabledDomain: Int) { }

  // CHECK: sil {{.*}} @$s4main1SVAA1PA2aDP13dynamicDomainxSi_tcfCTW : $@convention(witness_method: P) (Int, @thick S.Type) -> @out S
  @available(DynamicDomain)
  public init(dynamicDomain: Int) { }

  // CHECK: sil {{.*}} @$s4main1SVAA1PA2aDP19alwaysAvailableFuncyyFTW : $@convention(witness_method: P) (@in_guaranteed S) -> ()
  public func alwaysAvailableFunc() { }

  // CHECK: sil {{.*}} @$s4main1SVAA1PA2aDP17enabledDomainFuncyyFTW : $@convention(witness_method: P) (@in_guaranteed S) -> ()
  @available(EnabledDomain)
  public func enabledDomainFunc() { }

  // CHECK-NOT: @$s4main1SVAA1PA2aDP18disabledDomainFuncyyFTW
  @available(DisabledDomain)
  public func disabledDomainFunc() { }

  // CHECK: sil {{.*}} @$s4main1SVAA1PA2aDP17dynamicDomainFuncyyFTW : $@convention(witness_method: P) (@in_guaranteed S) -> ()
  @available(DynamicDomain)
  public func dynamicDomainFunc() { }

  // CHECK: sil {{.*}} @$s4main1SVAA1PA2aDP18alwaysAvailableVarSivgTW : $@convention(witness_method: P) (@in_guaranteed S) -> Int
  public var alwaysAvailableVar: Int { 0 }

  // CHECK: sil {{.*}} @$s4main1SVAA1PA2aDP16enabledDomainVarSivgTW : $@convention(witness_method: P) (@in_guaranteed S) -> Int
  @available(EnabledDomain)
  public var enabledDomainVar: Int { 0 }

  // CHECK-NOT: @$$s4main1SVAA1PA2aDP17disabledDomainVarSivgTW
  @available(DisabledDomain)
  public var disabledDomainVar: Int { 0 }

  // CHECK: sil {{.*}} @$s4main1SVAA1PA2aDP16dynamicDomainVarSivgTW : $@convention(witness_method: P) (@in_guaranteed S) -> Int
  @available(DynamicDomain)
  public var dynamicDomainVar: Int { 0 }
}

// CHECK-LABEL: sil_witness_table [serialized] S: P module main {
// CHECK-NEXT:    method #P.init!allocator: {{.*}} @$s4main1SVAA1PA2aDP15alwaysAvailablexSi_tcfCTW
// CHECK-NEXT:    method #P.init!allocator: {{.*}} @$s4main1SVAA1PA2aDP13enabledDomainxSi_tcfCTW
// CHECK-NEXT:    method #P.init!allocator: {{.*}} @$s4main1SVAA1PA2aDP13dynamicDomainxSi_tcfCTW
// CHECK-NEXT:    method #P.alwaysAvailableFunc: {{.*}} @$s4main1SVAA1PA2aDP19alwaysAvailableFuncyyFTW
// CHECK-NEXT:    method #P.enabledDomainFunc: {{.*}} @$s4main1SVAA1PA2aDP17enabledDomainFuncyyFTW
// CHECK-NEXT:    method #P.dynamicDomainFunc: {{.*}} @$s4main1SVAA1PA2aDP17dynamicDomainFuncyyFTW
// CHECK-NEXT:    method #P.alwaysAvailableVar!getter: {{.*}} @$s4main1SVAA1PA2aDP18alwaysAvailableVarSivgTW
// CHECK-NEXT:    method #P.enabledDomainVar!getter: {{.*}} @$s4main1SVAA1PA2aDP16enabledDomainVarSivgTW
// CHECK-NEXT:    method #P.dynamicDomainVar!getter: {{.*}} @$s4main1SVAA1PA2aDP16dynamicDomainVarSivgTW
// CHECK-NEXT:  }
