// RUN: not %swift %s -parse
// RUN: not %swift -parse-stdlib %s -parse

// Just don't crash when accidentally importing "SWIFT" instead of "Swift".

import SWIFT

print("hi")
SWIFT.print("hi")
