// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import MemberOutOfLine

public func add(_ lhs: inout LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs + rhs }

// CHECK: bb0([[LHS:%.*]] : $*LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [static] [[LHS]] : $*LoadableIntWrapper
// CHECK:   [[FUNC:%.*]] = function_ref [[NAME:@_ZNK18LoadableIntWrapperplES_]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK:   apply [[FUNC]]([[ACCESS]], [[RHS]]) : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK:   end_access [[ACCESS]] : $*LoadableIntWrapper

// CHECK: sil [clang LoadableIntWrapper."+"] [[NAME]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
