// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop -Xcc -fignore-exceptions | %FileCheck %s

import NonMemberOutOfLine

public func add(_ lhs: LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs + rhs }

// CHECK: call {{i32|i64}} [[NAME:@(_Zpl18LoadableIntWrapperS_|"\?\?H@YA\?AULoadableIntWrapper@@U0@0@Z")]]({{i32|\[1 x i32\]|i64}} %{{[0-9]+}}, {{i32|\[1 x i32\]|i64}} %{{[0-9]+}})
// CHECK: declare {{(dso_local )?}}{{i32|i64}} [[NAME]]({{i32|\[1 x i32\]|i64}}, {{i32|\[1 x i32\]|i64}})
