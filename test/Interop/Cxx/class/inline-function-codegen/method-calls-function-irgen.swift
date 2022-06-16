// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import MethodCallsFunction

public func getValueFromMethod() -> CInt {
  return callMethod(41)
}

// CHECK: define linkonce_odr{{( dso_local)?}} i32 @{{_Z9incrementi|"\?increment@@YAHH@Z"}}(i32 %t)
