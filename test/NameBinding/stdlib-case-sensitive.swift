// RUN: not %target-swift-frontend %s -parse
// RUN: not %target-swift-frontend -parse-stdlib %s -parse

// Just don't crash when accidentally importing "SWIFT" instead of "Swift".

import SWIFT

print("hi")
SWIFT.print("hi")
