// RUN: %target-typecheck-verify-swift

// rdar://83717297
//
// Make sure that trailing closures are matches to the `callAsFunction`
// when used in the same expression as `.init` of a callable type.

protocol View {}
struct EmptyView: View {}

struct MyLayout {
  func callAsFunction<V: View>(content: () -> V) -> MyLayout { .init() }
  func callAsFunction<V: View>(answer: () -> Int,
                               content: () -> V) -> MyLayout { .init() }
}

struct Test {
  var body1: MyLayout {
    MyLayout() {
      EmptyView() // Ok
    }
  }

  var body2: MyLayout {
    MyLayout() {
      42
    } content: {
      EmptyView() // Ok
    }
  }
}

// rdar://92912878 - filtering prevents disambiguation of `.callAsFunction`
func test_no_filtering_of_overloads() {
  struct S {
    init() {}
    init(_: String) {}

    func callAsFunction<T>(_ fn: () -> T) -> T {
      fn()
    }
  }

  func test(_: () -> Void) {
  }

  test {
    _ = S() { // Ok
      _ = 42
      print("Hello")
    }
  }
}
