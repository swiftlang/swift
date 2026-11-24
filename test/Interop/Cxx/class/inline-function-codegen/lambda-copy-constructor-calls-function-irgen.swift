// RUN: %target-swift-emit-ir %s -I %S/Inputs -cxx-interoperability-mode=default | %FileCheck %s

import LambdaCopyConstructorCallsFunction

public func copyLambda() -> CInt {
  return IntLambdaCopier().copyLambda()
}

// CHECK: define {{.*}}@{{_ZN11CopyCounterC[12]ERKS_|"\?\?0CopyCounter@@QEAA@AEBU0@@Z"}}(
