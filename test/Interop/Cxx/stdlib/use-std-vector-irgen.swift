// RUN: %target-swift-frontend -emit-irgen %s -I %S/Inputs -cxx-interoperability-mode=default -target %target-cpu-apple-macos26.0 | %FileCheck %s --check-prefix=CHECK-macosx-26
// RUN: %target-swift-frontend -emit-irgen %s -I %S/Inputs -cxx-interoperability-mode=default -target %target-cpu-apple-macos27.0 | %FileCheck %s --check-prefix=CHECK-macosx-27

// REQUIRES: OS=macosx

import StdVector
import CxxStdlib

public func genericUnderestimatedCount(_ v: Vector) -> Int {
  let s: any Sequence<CInt> = v
  return s.underestimatedCount
}

public func genericContains(_ v: Vector) -> Bool {
  let s: any Sequence<CInt> = v
  return s.contains(2)
}

// Make sure we never pick the Iterable witness table to resolve `underestimatedCount` and `contains`
// CHECK-macosx-26-LABEL: define{{.*}}VSTSCST19underestimatedCount
// CHECK-macosx-26-NOT: ${{.*}}Iterable{{.*}}underestimatedCount
// CHECK-macosx-26: call swiftcc i64 @"$sSlsE19underestimatedCountSivg"

// CHECK-macosx-26-LABEL: define{{.*}}VSTSCST31_customContainsEquatableElement
// CHECK-macosx-26-NOT: ${{.*}}Iterable{{.*}}customContainsEquatableElement
// CHECK-macosx-26: call swiftcc i8 @"$sSTsE31_customContainsEquatableElementySbSg0D0QzF"


// CHECK-macosx-27-LABEL: define{{.*}}VSTSCST19underestimatedCount
// CHECK-macosx-27: call swiftcc i64 @"${{.*}}Iterable{{.*}}underestimatedCount

// CHECK-macosx-27-LABEL: define{{.*}}VSTSCST31_customContainsEquatableElement
// CHECK-macosx-27: call swiftcc i8 @"${{.*}}Iterable{{.*}}customContainsEquatableElementy
