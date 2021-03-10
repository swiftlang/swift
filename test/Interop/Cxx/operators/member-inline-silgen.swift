// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import MemberInline

public func sub(_ lhs: inout LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs - rhs }

// CHECK: bb0([[SELF:%.*]] : $*LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):

// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*LoadableIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN18LoadableIntWrappermiES_|\?\?GLoadableIntWrapper@@QEAA\?AU0@U0@@Z)]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: apply [[OP]]([[SELFACCESS]], [[RHS]]) : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: end_access [[SELFACCESS]] : $*LoadableIntWrapper

// CHECK: sil [clang LoadableIntWrapper."-"] [[NAME]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper

public func call(_ wrapper: inout LoadableIntWrapper, _ arg: Int32) -> Int32 { wrapper(arg) }

// CHECK: bb0([[SELF:%.*]] : $*LoadableIntWrapper, [[RHS:%.*]] : $Int32):
// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*LoadableIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN18LoadableIntWrapperclEi|\?\?RLoadableIntWrapper@@QEAAHH@Z)]] : $@convention(c) (@inout LoadableIntWrapper, Int32) -> Int32
// CHECK: apply [[OP]]([[SELFACCESS]], [[RHS]]) : $@convention(c) (@inout LoadableIntWrapper, Int32) -> Int32
// CHECK: end_access [[SELFACCESS]] : $*LoadableIntWrapper

// CHECK: sil [clang LoadableIntWrapper.callAsFunction] [[NAME]] : $@convention(c) (@inout LoadableIntWrapper, Int32) -> Int32

public func call(_ wrapper: inout AddressOnlyIntWrapper) -> Int32 { wrapper() }

// CHECK: bb0([[SELF:%.*]] : $*AddressOnlyIntWrapper):
// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*AddressOnlyIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN21AddressOnlyIntWrapperclEv|\?\?RAddressOnlyIntWrapper@@QEAAHXZ)]] : $@convention(c) (@inout AddressOnlyIntWrapper) -> Int32
// CHECK: apply [[OP]]([[SELFACCESS]]) : $@convention(c) (@inout AddressOnlyIntWrapper) -> Int32
// CHECK: end_access [[SELFACCESS]] : $*AddressOnlyIntWrapper

// CHECK: sil [clang AddressOnlyIntWrapper.callAsFunction] [[NAME]] : $@convention(c) (@inout AddressOnlyIntWrapper) -> Int32
