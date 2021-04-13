// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import NonMemberOutOfLine

public func add(_ lhs: LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs + rhs }

// CHECK: [[COUNTER:%.*]] = function_ref [[NAME:@(_Zpl18LoadableIntWrapperS_|\?\?H@YA\?AULoadableIntWrapper@@U0@0@Z)]] : $@convention(c) (LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: apply [[COUNTER]](%0, %1) : $@convention(c) (LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper

// CHECK: sil [clang "+"] [[NAME]] : $@convention(c) (LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
