// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop -Xcc -fignore-exceptions | %FileCheck %s -check-prefix CHECK-%target-abi

import MemberOutOfLine

public func add(_ lhs: inout LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs + rhs }

// CHECK-SYSV: call {{i32|i64}} [[NAME:@_ZNK18LoadableIntWrapperplES_]](ptr %{{[0-9]+}}, {{i32|\[1 x i32\]|i64|%struct.LoadableIntWrapper\* byval\(.*\)}}{{.*}})
// CHECK-SYSV: declare {{.*}}{{i32|i64}} [[NAME]](ptr {{.*}}, {{i32|\[1 x i32\]|i64|%struct.LoadableIntWrapper\* .*byval\(%struct.LoadableIntWrapper\)}}{{.*}})

// CHECK-WIN: call void [[NAME:@"\?\?HLoadableIntWrapper@@QEBA\?AU0@U0@@Z"]](ptr %{{[0-9]+}}, ptr {{.*}} sret(%TSo18LoadableIntWrapperV) {{.*}}, i32 %{{[0-9]+}})
// CHECK-WIN: declare dso_local void [[NAME]](ptr {{.*}}, ptr {{.*}} sret(%struct.LoadableIntWrapper) {{.*}}, i32)
