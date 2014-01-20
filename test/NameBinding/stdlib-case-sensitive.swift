// RUN: not %swift %s -parse
// RUN: not %swift -parse-stdlib %s -parse

// Just don't crash when accidentally importing "Swift" instead of "swift".

import Swift

print("hi")
Swift.print("hi")
