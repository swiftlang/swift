// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -wmo -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

typealias ClockRead = (UnsafeMutablePointer<Int64>, UnsafeMutablePointer<Int64>) -> Void

func nanoseconds(_ read: ClockRead) -> Int64 {
  var seconds: Int64 = 0
  var nanoseconds: Int64 = 0
  read(&seconds, &nanoseconds)
  precondition(nanoseconds >= 0 && nanoseconds < 1_000_000_000)
  return seconds * 1_000_000_000 + nanoseconds
}

@main
struct Main {
  static func main() {
    // The suspending clock's interval encloses the continuous clock's.
    let suspendingStart = nanoseconds(_swift_clockSuspending_getTime)
    let continuousStart = nanoseconds(_swift_clockContinuous_getTime)
    var continuousElapsed: Int64 = 0
    var reads = 0
    while continuousElapsed < 51_000_000 && reads < 100_000_000 {
      continuousElapsed = nanoseconds(_swift_clockContinuous_getTime) - continuousStart
      reads += 1
    }
    let suspendingElapsed = nanoseconds(_swift_clockSuspending_getTime) - suspendingStart
    print(continuousElapsed >= 51_000_000 ? "continuous advances: ok" : "continuous advances: stuck")
    // CHECK: continuous advances: ok
    print(suspendingElapsed >= 50_000_000 ? "suspending advances: ok" : "suspending advances: short")
    // CHECK-NEXT: suspending advances: ok

    let continuousResolution = nanoseconds(_swift_clockContinuous_getResolution)
    print(continuousResolution > 0 ? "continuous resolution: ok" : "continuous resolution: zero")
    // CHECK-NEXT: continuous resolution: ok
    let suspendingResolution = nanoseconds(_swift_clockSuspending_getResolution)
    print(suspendingResolution > 0 ? "suspending resolution: ok" : "suspending resolution: zero")
    // CHECK-NEXT: suspending resolution: ok
  }
}
