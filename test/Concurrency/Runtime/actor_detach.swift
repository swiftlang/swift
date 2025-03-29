// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple) | %FileCheck %s

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
func test() {
    detach {
        let x = await Manager.shared.manage()
        print(x)
    }
    detach {
        let x = await Manager.shared.other()
        print(x)
    }
}

if #available(SwiftStdlib 5.1, *) {
 test()
 sleep(30)
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
