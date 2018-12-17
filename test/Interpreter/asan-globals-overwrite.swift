// RUN: %target-build-swift -O -whole-module-optimization -sanitize=address %s -o %t
// RUN: %target-codesign %t
// RUN: %target-run %t

// REQUIRES: executable_test
// REQUIRES: objc_interop

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
