if case let x? = Optional.some(5) {}
if case let y: Int? = Optional.some(5) {}
guard case let z: String? = Optional.some("a") else { fatalError() }
while case let w: String? = Optional.some("b") {}

enum Pair<U, V> {
  case pair(U, V)
}

switch Pair.pair(Optional.some(0), "test") {
case .pair(let u, var v):
  break
}

// RUN: %sourcekitd-test -req=collect-var-type %s -- %s | %FileCheck %s
// CHECK: (1:13, 1:14): Int (explicit type: 0)
// CHECK: (2:13, 2:14): Int? (explicit type: 1)
// CHECK: (3:16, 3:17): String? (explicit type: 1)
// CHECK: (4:16, 4:17): String? (explicit type: 1)
// CHECK: (11:16, 11:17): Int? (explicit type: 0)
// CHECK: (11:23, 11:24): String (explicit type: 0)
