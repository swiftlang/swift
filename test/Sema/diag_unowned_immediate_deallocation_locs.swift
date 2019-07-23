// RUN: %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s

// Ensure we emit the warnings on the equal '=' tokens.
class C {}

weak var c1 = C() // CHECK: [[@LINE]]:13: warning: instance will be immediately deallocated because variable 'c1' is 'weak'

weak var c2: C? = C() // CHECK: [[@LINE]]:17: warning: instance will be immediately deallocated because variable 'c2' is 'weak'

weak var (c3, c4) = (C(), C())
// CHECK: [[@LINE-1]]:19: warning: instance will be immediately deallocated because variable 'c3' is 'weak'
// CHECK: [[@LINE-2]]:19: warning: instance will be immediately deallocated because variable 'c4' is 'weak'

weak var (c5, c6): (C?, C?) = (C(), C())
// CHECK: [[@LINE-1]]:29: warning: instance will be immediately deallocated because variable 'c5' is 'weak'
// CHECK: [[@LINE-2]]:29: warning: instance will be immediately deallocated because variable 'c6' is 'weak'

unowned let c7 = C(), c8: C = C()
// CHECK: [[@LINE-1]]:16: warning: instance will be immediately deallocated because variable 'c7' is 'unowned'
// CHECK: [[@LINE-2]]:29: warning: instance will be immediately deallocated because variable 'c8' is 'unowned'

c1 = C() // CHECK: [[@LINE]]:4: warning: instance will be immediately deallocated because variable 'c1' is 'weak'
