// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Unavailable -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Unavailable -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Unavailable.symbols.json

// REQUIRES: OS=macosx

public struct ShouldAppear {}

// CHECK-NOT: OSXUnavailable
@available(OSX, unavailable)
public struct OSXUnavailable {}

@available(OSX, unavailable)
extension ShouldAppear {
  public func shouldntAppear1() {}
}

// CHECK-NOT: OSXObsoleted
@available(OSX, obsoleted: 10.9)
public struct OSXObsoleted {}

@available(OSX, obsoleted: 10.9)
extension ShouldAppear {
  public func shouldntAppear2() {}
}

// CHECK-NOT: SwiftObsoleted
@available(swift, obsoleted: 1.0)
public struct SwiftObsoleted {}

@available(swift, obsoleted: 1.0)
extension ShouldAppear {
  public func shouldntAppear3() {}
}

// CHECK-NOT: shouldntAppear
