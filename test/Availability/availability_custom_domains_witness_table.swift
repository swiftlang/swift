// REQUIRES: swift_feature_CustomAvailability

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen -module-name main %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -O | %FileCheck %s --check-prefixes=CHECK

// Method implementation for S.requirement() shouldn't be emitted.
// CHECK-NOT: @$s4main1SV11requirementyyF
// CHECK: sil [ossa] @$s4main1SV20availableRequirementyyF : $@convention(method) (S) -> () {
// CHECK: sil [ossa] @$s4main1SV21availableRequirement2yyF : $@convention(method) (S) -> () {

// Witness thunk from S.requirement() to P.requirement() shouldn't be emitted.
// CHECK-NOT: @$s4main1SVAA1PA2aDP11requirementyyFTW
// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s4main1SVAA1PA2aDP20availableRequirementyyFTW : $@convention(witness_method: P) (@in_guaranteed S) -> () {
// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s4main1SVAA1PA2aDP21availableRequirement2yyFTW : $@convention(witness_method: P) (@in_guaranteed S) -> () {

// Witness from S.requirement() to P.requirement() shouldn't be emitted.
// CHECK-LABEL: sil_witness_table [serialized] S: P module main {
// CHECK-NOT: @$s4main1SVAA1PA2aDP11requirementyyFTW
// CHECK: method #P.availableRequirement: <Self where Self : P> (Self) -> () -> () : @$s4main1SVAA1PA2aDP20availableRequirementyyFTW    // protocol witness for P.availableRequirement() in conformance S
// CHECK: method #P.availableRequirement2: <Self where Self : P> (Self) -> () -> () : @$s4main1SVAA1PA2aDP21availableRequirement2yyFTW  // protocol witness for P.availableRequirement2() in conformance S

public protocol P {
    @available(DisabledDomain)
    func requirement()
    func availableRequirement()
    @available(EnabledDomain)
    func availableRequirement2()
}

public struct S: P {
    @available(DisabledDomain)
    public func requirement() { }
    public func availableRequirement() { }
    public func availableRequirement2() { }
    public init() { }
}

public func test() {
    if #available(DisabledDomain) {
        S().requirement()
    }
}
