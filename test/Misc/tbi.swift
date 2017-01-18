// RUN: %target-build-swift -target arm64-apple-ios8.0 -target-cpu cyclone \
// RUN:   -O -S %s -parse-as-library -parse-stdlib | \
// RUN:   %FileCheck --check-prefix=TBI %s

// RUN: %target-build-swift -target arm64-apple-ios7.0 -target-cpu cyclone \
// RUN:     -O -S %s -parse-as-library -parse-stdlib | \
// RUN:   %FileCheck --check-prefix=NO_TBI %s

// REQUIRES: CPU=arm64, OS=ios

// Verify that TBI is on by default in Swift on targets that support it. For our
// purposes this means iOS8.0 or later.

import Swift

func f(i: Int) -> Int8 {
  let j = i & 0xff_ffff_ffff_ffff
// TBI-NOT: and
// NO_TBI: and
  guard let p = UnsafeMutablePointer<Int8>(bitPattern: j) else {
    // Make sure we optimize away this branch.
    Builtin.unreachable()
  }
  return p[0]
}
