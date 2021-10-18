// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import MemberOutOfLine

public func add(_ lhs: inout LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs + rhs }

// CHECK: call {{i32|i64}} [[NAME:@_ZNK18LoadableIntWrapperplES_]](%struct.LoadableIntWrapper* %{{[0-9]+}}, {{i32|\[1 x i32\]|i64|%struct.LoadableIntWrapper\* byval\(.*\) align 4}} %{{[0-9]+}})
// CHECK: declare {{(dso_local )?}}{{i32|i64}} [[NAME]](%struct.LoadableIntWrapper* nonnull align 4 dereferenceable(4), {{i32|\[1 x i32\]|i64|%struct.LoadableIntWrapper\* byval\(%struct.LoadableIntWrapper\) align 4}})
