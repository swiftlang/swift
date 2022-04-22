// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s -check-prefix CHECK-%target-abi

// XFAIL: *

import MemberOutOfLine

public func add(_ lhs: LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs + rhs }

// CHECK-SYSV: bb0([[LHS:%.*]] : $LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):
// CHECK-SYSV:   [[FUNC:%.*]] = function_ref [[NAME:@_ZNK18LoadableIntWrapperplES_]] : $@convention(c) (@in LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-SYSV:   apply [[FUNC]]([[ACCESS:%.*]], [[RHS]]) : $@convention(c) (@in LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper

// CHECK-SYSV: sil [clang LoadableIntWrapper."+"] [[NAME]] : $@convention(c) (@in LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper

// CHECK-WIN: bb0([[LHS:%.*]] : $LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):
// CHECK-WIN:   [[FUNC:%.*]] = function_ref [[NAME:@\?\?HLoadableIntWrapper@@QEBA\?AU0@U0@@Z]] : $@convention(c) (@in LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-WIN:   apply [[FUNC]]([[ACCESS:%.*]], [[RHS]]) : $@convention(c) (@in LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper

// CHECK-WIN: sil [clang LoadableIntWrapper."+"] [[NAME]] : $@convention(c) (@in LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
