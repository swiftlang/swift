// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import MemberInline

public func sub(_ lhs: inout LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs - rhs }

// CHECK: bb0([[SELF:%.*]] : $*LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):

// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*LoadableIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN18LoadableIntWrappermiES_|\?\?GLoadableIntWrapper@@QEAA\?AU0@U0@@Z)]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: apply [[OP]]([[SELFACCESS]], [[RHS]]) : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: end_access [[SELFACCESS]] : $*LoadableIntWrapper

// CHECK: sil [clang LoadableIntWrapper."-"] [[NAME]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper

// CHECK-LABEL: sil @$s4main24subAddressOnlyIntWrapperySo0cdeF0VADz_ADtF : $@convention(thin) (@inout AddressOnlyIntWrapper, @in_guaranteed AddressOnlyIntWrapper) -> @out AddressOnlyIntWrapper
// CHECK: bb0([[OUT:%.*]] : $*AddressOnlyIntWrapper, [[LHS:%.*]] : $*AddressOnlyIntWrapper, [[RHS:%.*]] : $*AddressOnlyIntWrapper):
// CHECK: [[RHS_TMP:%.*]] = alloc_stack $AddressOnlyIntWrapper
// CHECK: copy_addr [[RHS]] to [initialization] [[RHS_TMP]] : $*AddressOnlyIntWrapper
// CHECK: [[LHS_ACCESS:%.*]] = begin_access [modify] [static] [[LHS]] : $*AddressOnlyIntWrapper
// CHECK: [[FN:%.*]] = function_ref [[NAME:@(_ZN21AddressOnlyIntWrappermiES_|\?\?GAddressOnlyIntWrapper@@QEAA\?AU0@U0@@Z)]] : $@convention(c) (@inout AddressOnlyIntWrapper, @in AddressOnlyIntWrapper) -> @out AddressOnlyIntWrapper
// CHECK: apply [[FN]]([[OUT]], [[LHS_ACCESS]], [[RHS_TMP]]) : $@convention(c) (@inout AddressOnlyIntWrapper, @in AddressOnlyIntWrapper) -> @out AddressOnlyIntWrapper
// CHECK: end_access [[LHS_ACCESS]]
// CHECK: dealloc_stack [[RHS_TMP]]
// CHECK: end sil function '$s4main24subAddressOnlyIntWrapperySo0cdeF0VADz_ADtF'

// CHECK: sil [clang AddressOnlyIntWrapper."-"] [[NAME]] : $@convention(c) (@inout AddressOnlyIntWrapper, @in AddressOnlyIntWrapper) -> @out AddressOnlyIntWrapper
public func subAddressOnlyIntWrapper(_ lhs: inout AddressOnlyIntWrapper, _ rhs: AddressOnlyIntWrapper) -> AddressOnlyIntWrapper { lhs - rhs }
