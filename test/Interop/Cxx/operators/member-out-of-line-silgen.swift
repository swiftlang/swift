// RUN: %target-swift-emit-sil -Xllvm -sil-print-types %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s -check-prefix CHECK-%target-abi

import MemberOutOfLine

public func add(_ lhs: LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs + rhs }

// CHECK-SYSV: bb0([[LHS:%.*]] : $LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):
// CHECK-SYSV:   store [[LHS]] to [[STORE_LOC:%.*]] : $*LoadableIntWrapper
// CHECK-SYSV:   [[FUNC:%.*]] = function_ref [[NAME:@_ZNK18LoadableIntWrapperplES_]] : $@convention(cxx_method) (LoadableIntWrapper, @in_guaranteed LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-SYSV:   apply [[FUNC]]([[ACCESS:%.*]], [[STORE_LOC]]) : $@convention(cxx_method) (LoadableIntWrapper, @in_guaranteed LoadableIntWrapper) -> LoadableIntWrapper

// CHECK-SYSV: sil [clang LoadableIntWrapper.__operatorPlus] [[NAME]] : $@convention(cxx_method) (LoadableIntWrapper, @in_guaranteed LoadableIntWrapper) -> LoadableIntWrapper

// CHECK-WIN: bb0([[LHS:%.*]] : $LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):
// CHECK-WIN:   store [[LHS]] to [[STORE_LOC:%.*]] : $*LoadableIntWrapper
// CHECK-WIN:   [[FUNC:%.*]] = function_ref [[NAME:@\?\?HLoadableIntWrapper@@QEBA\?AU0@U0@@Z]] : $@convention(cxx_method) (LoadableIntWrapper, @in_guaranteed LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-WIN:   apply [[FUNC]]([[ACCESS:%.*]], [[STORE_LOC]]) : $@convention(cxx_method) (LoadableIntWrapper, @in_guaranteed LoadableIntWrapper) -> LoadableIntWrapper

// CHECK-WIN: sil [clang LoadableIntWrapper.__operatorPlus] [[NAME]] : $@convention(cxx_method) (LoadableIntWrapper, @in_guaranteed LoadableIntWrapper) -> LoadableIntWrapper
