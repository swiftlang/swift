// RUN: not %target-swift-frontend %s -typecheck

struct A<T> {
  private var b: [T]
}

func foo(v: [Int]) {
  let _ = A(b: v.sorted { $0 < $1 }.map{ $0 })
}
