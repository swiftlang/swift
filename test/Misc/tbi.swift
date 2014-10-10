// RUN: %target-build-swift -target arm64-apple-ios8.0 -target-cpu cyclone \
// RUN:                          -O -emit-ir %s -parse-as-library | \
// RUN:   FileCheck --check-prefix=BOTH --check-prefix=TBI %s

// RUN: %target-build-swift -target arm64-apple-ios8.0 -target-cpu cyclone \
// RUN:     -Xcc -Xclang -Xcc -target-feature -Xcc -Xclang -Xcc -tbi \
// RUN:     -O -emit-ir %s -parse-as-library | \
// RUN:   FileCheck --check-prefix=BOTH --check-prefix=NO_TBI %s

// REQUIRES: CPU=arm64, OS=ios

// Verify that TBI is on by default in Swift.

func f(i: Int) -> Int8 {
  let j = i & 0xff_ffff_ffff_ffff
// TBI-NOT: and
// NO_TBI: and
  let p = UnsafeMutablePointer<Int8>(bitPattern: j)
  return p[0]
}
