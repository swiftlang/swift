// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx12 %s -o %t/linking_direct
// RUN: %target-build-swift -target %target-cpu-apple-macosx11 %s -o %t/linking_rpath
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 %s -o %t/linking_rpath_old

// RUN: otool -L %t/linking_direct | %FileCheck -check-prefix CHECK-DIRECT %s
// RUN: otool -L %t/linking_rpath | %FileCheck -check-prefix CHECK-RPATH %s
// RUN: otool -L %t/linking_rpath_old | %FileCheck -check-prefix CHECK-RPATH %s

// REQUIRES: OS=macosx

// CHECK-DIRECT: /usr/lib/swift/libswift_Concurrency.dylib
// CHECK-RPATH: @rpath/libswift_Concurrency.dylib

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public struct X {
  public func f() async -> Int { return 0 }
  public func g() async -> Int {
    await f()
  }
}
