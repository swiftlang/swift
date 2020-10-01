// RUN: %target-run-simple-swift(-O -whole-module-optimization -sanitize=address)

// REQUIRES: executable_test
// REQUIRES: foundation
// REQUIRES: asan_runtime

import Foundation

struct MyStruct {
    let value = Data()
}

class MyClass: NSObject {
    var x: Int = {
        _ = MyStruct()
        return 0
    }()
}
