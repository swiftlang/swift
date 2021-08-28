guard var opt: Int = .some(23) else { fatalError() }

func foo() {
  guard let x = .some("abc") else { return }
  guard let y: String = .some("def") else { return }

  if let z: Int = .some(4) {}

  while var w: Int = .some(5) {}
}

// RUN: %sourcekitd-test -req=collect-var-type %s -- %s | %FileCheck %s
// CHECK: (1:11, 1:14): Int (explicit type: 1)
// CHECK: (4:13, 4:14): String (explicit type: 0)
// CHECK: (5:13, 5:14): String (explicit type: 1)
// CHECK: (7:10, 7:11): Int (explicit type: 1)
// CHECK: (9:13, 9:14): Int (explicit type: 1)
