// Ensure scope construction does not crash on this illegal code
// RUN: not %target-swift-frontend -typecheck %s 2> %t.errors
// RUN: %FileCheck %s <%t.errors
// CHECK-NOT: Program arguments:

// Note: extra newlines below ensure that context printing doesn't show the
// CHECK-NOT line.




private extension String {
  private extension String
