// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import MemberOutOfLine

public func add(_ lhs: LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs + rhs }

// CHECK: bb0([[LHS:%.*]] : $LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):
// CHECK:   [[FUNC:%.*]] = function_ref [[NAME:@_ZNK18LoadableIntWrapperplES_]] : $@convention(c) (@in LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK:   apply [[FUNC]]([[ACCESS:%.*]], [[RHS]]) : $@convention(c) (@in LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper

// CHECK: sil [clang LoadableIntWrapper."+"] [[NAME]] : $@convention(c) (@in LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
