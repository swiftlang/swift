// RUN: %target-swift-emit-silgen %s -enable-experimental-distributed -target %target-swift-5.7-abi-triple -debug-forbid-typecheck-prefix NEVER_TYPECHECK -experimental-skip-non-inlinable-function-bodies | %FileCheck %s --check-prefixes=CHECK,CHECK-SKIP-NONINLINE
// RUN: %target-swift-emit-silgen %s -enable-experimental-distributed -target %target-swift-5.7-abi-triple -debug-forbid-typecheck-prefix NEVER_TYPECHECK -debug-forbid-typecheck-prefix SKIP_ALL_NO_TYPECHECK -experimental-skip-all-function-bodies | %FileCheck %s --check-prefixes=CHECK,CHECK-SKIP-ALL

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

public distributed actor DA {
  public typealias ActorSystem = LocalTestingDistributedActorSystem

  // CHECK-NOT: s38distributed_thunk_skip_function_bodies2DAC9publicVarSiyYaKFTE
  distributed var publicVar: Int {
    let NEVER_TYPECHECK = 1
    blackHole(NEVER_TYPECHECK)
    return 1
  }
}

@inline(never)
public func blackHole<T>(_ t: T) { }

extension DA {
  // CHECK-NOT: s38distributed_thunk_skip_function_bodies2DAC20publicVarInExtensionSiyYaKFTE
  distributed var publicVarInExtension: Int {
    let NEVER_TYPECHECK = 1
    blackHole(NEVER_TYPECHECK)
    return 1
  }

  // CHECK-NOT: s38distributed_thunk_skip_function_bodies2DAC10publicFuncyyYaKFTE
  public distributed func publicFunc() {
    let NEVER_TYPECHECK = 1
    blackHole(NEVER_TYPECHECK)
  }

  // CHECK-SKIP-ALL-NOT: s38distributed_thunk_skip_function_bodies2DAC13inlinableFuncyyF

  // CHECK-SKIP-NONINLINE-LABEL: sil [serialized] [distributed] [ossa] @$s38distributed_thunk_skip_function_bodies2DAC13inlinableFuncyyF : $@convention(method) (@sil_isolated @guaranteed DA) -> () {
  // CHECK-SKIP-NONINLINE:         function_ref @$s38distributed_thunk_skip_function_bodies9blackHoleyyxlF
  // CHECK-SKIP-NONINLINE:       } // end sil function '$s38distributed_thunk_skip_function_bodies2DAC13inlinableFuncyyF'
  @inlinable public distributed func inlinableFunc() {
    let SKIP_ALL_NO_TYPECHECK = 1
    blackHole(SKIP_ALL_NO_TYPECHECK)
  }
}
