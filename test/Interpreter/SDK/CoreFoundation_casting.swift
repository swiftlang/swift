// RUN: %target-run-simple-swift

import CoreFoundation

class SwiftClass { }

// Check _cfTypeID()
let nsObject = NSObject()
let swiftObject = SwiftClass()
assert(CFGetTypeID(nsObject) == CFGetTypeID(swiftObject))

