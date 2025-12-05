// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple) | %FileCheck %s
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)  | %FileCheck %s
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: foundation

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Foundation

@available(SwiftStdlib 5.1, *)
actor Manager {
    static var shared = Manager()

    func manage() async -> Int {
        print("manage")
        return 0
    }

    func other() async -> Int{
        print("other")
        return 0
    }
}


@available(SwiftStdlib 5.1, *)
func test() async -> [Task<(), Never>] {
    let t1 = Task.detached {
        let x = await Manager.shared.manage()
        print(x)
    }
    let t2 = Task.detached {
        let x = await Manager.shared.other()
        print(x)
    }

    return [t1, t2]
}

if #available(SwiftStdlib 5.1, *) {
  let ts = await test()

  for t in ts {
    await t.value
  }
} else {
 print("manage")
 print("0")
 print("other")
 print("0")
}
// CHECK-DAG: manage
// CHECK-DAG: 0
// CHECK-DAG: other
// CHECK-DAG: 0
