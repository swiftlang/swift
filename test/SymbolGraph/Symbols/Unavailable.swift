// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Unavailable -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Unavailable -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/Unavailable.symbols.json
// RUN: %{python} -c 'import os.path; import sys; sys.exit(1 if os.path.exists(sys.argv[1]) else 0)' %t/Unavailable@Swift.symbols.json

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

// CHECK-NOT: shouldntAppear

@available(OSX, unavailable)
extension String {
  public func shouldntAppear1() { }
}

@available(OSX, obsoleted: 10.9)
extension String {
  public func shouldntAppear2() {}
}
