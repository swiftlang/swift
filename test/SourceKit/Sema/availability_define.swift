// RUN: %sourcekitd-test -req=sema %s -- %s \
// RUN:  -Xfrontend -define-availability \
// RUN:  -Xfrontend "_iOS8Aligned:macOS 10.10, iOS 8.0" | %FileCheck %s

// REQUIRES: OS=macosx

@available(_iOS8Aligned, *)
public func onMacOS10_10() {}
// CHECK-NOT: key.line: 7,

@available(_iOS9Aligned, *)
public func onMacOS10_11() {}
// CHECK: key.line: 11,
// CHECK: key.column: 26,
// CHECK: key.description: "expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'",
