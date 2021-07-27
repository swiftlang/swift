// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: foundation

import Foundation

@available(SwiftStdlib 5.5, *)
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


@available(SwiftStdlib 5.5, *)
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

if #available(SwiftStdlib 5.5, *) {
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
