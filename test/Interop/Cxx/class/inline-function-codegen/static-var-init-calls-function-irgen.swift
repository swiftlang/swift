// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import StaticVarInitCallsFunction

public func getInitializedStaticVar() -> CInt {
  return initializeStaticVar()
}

// CHECK: define {{.*}}i32 @{{_Z9incrementi|"\?increment@@YAHH@Z"}}(i32{{.*}})
