// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import NonMemberOutOfLine

public func add(_ lhs: LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs + rhs }

// CHECK: [[COUNTER:%.*]] = function_ref @$sSo1poiySo18LoadableIntWrapperVAC_ACtFTo : $@convention(c) (LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: apply [[COUNTER]](%0, %1) : $@convention(c) (LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper

// CHECK: sil [asmname "{{(_Zpl18LoadableIntWrapperS_|\?\?H@YA\?AULoadableIntWrapper@@U0@0@Z)}}"] [clang "+"] @$sSo1poiySo18LoadableIntWrapperVAC_ACtFTo : $@convention(c) (LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
