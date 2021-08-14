// RUN: %target-typecheck-verify-swift

// rdar://70158735

@resultBuilder
struct A<T> {
  static func buildBlock(_ values: Int...) -> Int { return 0 }
}

struct B<T> {}

extension B {
  @resultBuilder
  struct Generic<U> {
    static func buildBlock(_ values: Int...) -> Int { return 0 }
  }

  @resultBuilder
  struct NonGeneric {
    static func buildBlock(_ values: Int...) -> Int { return 0 }
  }
}

@A<Float> var test0: Int {
  1
  2
  3
}

@B<Float>.NonGeneric var test1: Int {
  1
  2
  3
}

@B<Float>.Generic<Float> var test2: Int {
  1
  2
  3
}
