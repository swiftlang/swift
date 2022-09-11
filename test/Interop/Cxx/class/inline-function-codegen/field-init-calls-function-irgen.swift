// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import FieldInitCallsFunction

public func getInitializedField() -> CInt {
  return initializeField()
}

// CHECK: define {{.*}}i32 @{{_Z9incrementi|"\?increment@@YAHH@Z"}}(i32 {{.*}})
