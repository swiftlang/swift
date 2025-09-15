// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx12 %s -o %t/linking_direct 2>&1 | %FileCheck -allow-empty -check-prefix CHECK-BUILD-ERRORS %s
// RUN: %target-build-swift -target %target-cpu-apple-macosx11 %s -o %t/linking_rpath 2>&1 | %FileCheck -allow-empty -check-prefix CHECK-BUILD-ERRORS %s
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 %s -o %t/linking_rpath_old 2>&1 | %FileCheck -allow-empty -check-prefix CHECK-BUILD-ERRORS %s

// Make sure the linker didn't emit any version mismatch warnings.
// CHECK-BUILD-ERRORS-NOT: was built for newer 'macOS' version

// RUN: otool -L %t/linking_direct | %FileCheck -check-prefix CHECK-DIRECT %s
// RUN: otool -L %t/linking_rpath | %FileCheck -check-prefix CHECK-RPATH %s
// RUN: otool -L %t/linking_rpath_old | %FileCheck -check-prefix CHECK-RPATH %s

// RUN: %target-build-swift -disable-autolinking-runtime-compatibility-concurrency -target %target-cpu-apple-ios15.0-macabi %s -o %t/linking_direct
// RUN: %target-build-swift -disable-autolinking-runtime-compatibility-concurrency -target %target-cpu-apple-ios14.0-macabi %s -o %t/linking_rpath

// RUN: otool -L %t/linking_direct | %FileCheck -check-prefix CHECK-DIRECT %s
// RUN: otool -L %t/linking_rpath | %FileCheck -check-prefix CHECK-RPATH %s

// REQUIRES: OS=macosx
// REQUIRES: maccatalyst_support

// CHECK-DIRECT: /usr/lib/swift/libswift_Concurrency.dylib
// CHECK-RPATH: @rpath/libswift_Concurrency.dylib

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public struct X {
  public func f() async -> Int { return 0 }
  public func g() async -> Int {
    await f()
  }
}
