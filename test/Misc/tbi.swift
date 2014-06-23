// RUN: %target-build-swift -target arm64-apple-ios8.0 -target-cpu cyclone \
// RUN:                          -O3 -S %s -parse-as-library | \
// RUN:   FileCheck --check-prefix=BOTH --check-prefix=TBI %s

// RUN: %target-build-swift -target arm64-apple-ios8.0 -target-cpu cyclone \
// RUN:     -target-feature -tbi -O3 -S %s -parse-as-library | \
// RUN:   FileCheck --check-prefix=BOTH --check-prefix=NO_TBI %s

// REQUIRES: CPU=arm64, OS=ios

// Verify that TBI is on by default in Swift.

func f(i: Int) -> Int8 {
  let j = i & 0xff_ffff_ffff_ffff
// TBI-NOT: and
// NO_TBI: and
  let p = UnsafePointer<Int8>(j)
  return p[0]
}
