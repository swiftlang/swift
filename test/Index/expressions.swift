// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

protocol P1 {}

// CHECK: [[@LINE+1]]:8 | struct/Swift | S1 | [[S1_USR:.*]] | Def
struct S1 : P1 {}

func test(_ o: P1?) {
  switch o {
  // CHECK-NOT: [[@LINE+2]]:17 | enumerator/Swift | some |
  // CHECK: [[@LINE+1]]:17 | struct/Swift | S1 | [[S1_USR]] | Ref
  case let s as S1:
    test(s)
  }
}
