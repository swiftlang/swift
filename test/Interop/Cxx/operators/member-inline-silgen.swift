// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import MemberInline

public func sub(_ lhs: inout LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs - rhs }

// CHECK: bb0([[SELF:%.*]] : $*LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):

// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*LoadableIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN18LoadableIntWrappermiES_|\?\?GLoadableIntWrapper@@QEAA\?AU0@U0@@Z)]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: apply [[OP]]([[SELFACCESS]], [[RHS]]) : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: end_access [[SELFACCESS]] : $*LoadableIntWrapper

// CHECK: sil [clang LoadableIntWrapper."-"] [[NAME]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
