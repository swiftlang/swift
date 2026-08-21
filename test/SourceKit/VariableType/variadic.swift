func test(x: Int...) {
  let y = x
}

func generic<T>(x: T...) {
  let y = x
}

// RUN: %sourcekitd-test -req=collect-var-type %s -- %s | %FileCheck %s
// CHECK: (1:11, 1:12): [Int] (explicit type: 1)
// CHECK: (2:7, 2:8): [Int] (explicit type: 0)
// CHECK: (5:17, 5:18): [T] (explicit type: 1)
// CHECK: (6:7, 6:8): [T] (explicit type: 0)
