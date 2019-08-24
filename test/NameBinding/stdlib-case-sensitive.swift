// RUN: not %target-swift-frontend %s -typecheck
// RUN: not %target-swift-frontend -parse-stdlib %s -typecheck

// Just don't crash when accidentally importing "SWIFT" instead of "Swift".

import SWIFT

print("hi")
SWIFT.print("hi")
