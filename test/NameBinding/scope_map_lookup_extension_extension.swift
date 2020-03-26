// Ensure scope construction does not crash on this illegal code
// RUN: not %target-swift-frontend -typecheck %s 2> %t.errors
// RUN: %FileCheck %s <%t.errors
// CHECK-NOT: Program arguments:
private extension String {
  private extension String
