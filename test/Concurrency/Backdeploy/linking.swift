// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target x86_64-apple-macosx12 %s -o %t/linking_direct
// RUN: %target-build-swift -target x86_64-apple-macosx11 %s -o %t/linking_rpath
// RUN: %target-build-swift -target x86_64-apple-macosx10.10 %s -o %t/linking_rpath_old

// RUN: otool -L %t/linking_direct | %FileCheck -check-prefix CHECK-DIRECT %s
// RUN: otool -L %t/linking_rpath | %FileCheck -check-prefix CHECK-RPATH %s
// RUN: otool -L %t/linking_rpath_old | %FileCheck -check-prefix CHECK-RPATH %s

// RUN: %target-build-swift -disable-autolinking-runtime-compatibility-concurrency -target x86_64-apple-ios15.0-macabi %s -o %t/linking_direct
// RUN: %target-build-swift -disable-autolinking-runtime-compatibility-concurrency -target x86_64-apple-ios14.0-macabi %s -o %t/linking_rpath

// RUN: otool -L %t/linking_direct | %FileCheck -check-prefix CHECK-DIRECT %s
// RUN: otool -L %t/linking_rpath | %FileCheck -check-prefix CHECK-RPATH %s

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64
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
