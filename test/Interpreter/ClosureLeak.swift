// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

enum E {
    case a(String, Character, String)
}

func doit( action: () -> ()) {
  action()
}

func foo(_ s: [String]) {
    let bar = E.a(s[0], "a", "b")
    doit {
      let _ = bar
    }
 }

func getStrings() -> [String] {
    let c = "f.f".components(separatedBy: ".")
    return c
}

for _ in 0 ..< 2_000_000 {
  autoreleasepool {
    let c = getStrings()
    foo(c)
   }
}

var usage = rusage()
getrusage(RUSAGE_SELF, &usage)

// CHECK: success
// CHECK-NOT: failure

// We should not need 50MB for this.
print(usage.ru_maxrss/(1024*1024))
if usage.ru_maxrss > 50 * 1024 * 1024 {
  print("failure - should not need 50MB!")
} else {
  print("success")
}
