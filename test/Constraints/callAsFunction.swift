// RUN: %target-typecheck-verify-swift

// rdar://83717297
//
// Make sure that trailing closures are matches to the `callAsFunction`
// when used in the same expression as `.init` of a callable type.

protocol View {}
struct EmptyView: View {}

enum Align {
case top, center, bottom
}

struct MyLayout {
  init(alignment: Align? = .center, spacing: Double? = 0.0) {}

  func callAsFunction<V: View>(content: () -> V) -> MyLayout { .init() }
  // expected-note@-1 {{where 'V' = 'Int'}}
  func callAsFunction<V: View>(answer: () -> Int,
                               content: () -> V) -> MyLayout { .init() }
  // expected-note@-2 {{where 'V' = 'Int'}}
}

struct Test {
  var body1: MyLayout {
    MyLayout(spacing: 1.0) {
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

  var body3: MyLayout {
    MyLayout(alignment: .top) {
      let x = 42
      return x
    } content: {
      EmptyView() // Ok
    }
  }

  var body4: MyLayout {
    MyLayout(spacing: 1.0) {
      let x = 42
      return x
    } content: {
      _ = 42
      return EmptyView() // Ok
    }
  }

  var body5: MyLayout {
    MyLayout(alignment: .bottom, spacing: 1.0) {
      42
    } content: {
      EmptyView() // Ok
    }
  }

  var body6: MyLayout {
    MyLayout(spacing: 1.0) {
      _ = EmptyView()
      return 42
    } // expected-error {{instance method 'callAsFunction(content:)' requires that 'Int' conform to 'View'}}
  }

  var body7: MyLayout {
    MyLayout(alignment: .center) {
      42
    } content: {
      _ = EmptyView()
      return 42
    } // expected-error {{instance method 'callAsFunction(answer:content:)' requires that 'Int' conform to 'View'}}
  }

  var body8: MyLayout {
    MyLayout {
      let x = ""
      return x // expected-error {{cannot convert return expression of type 'String' to return type 'Int'}}
    } content: {
      EmptyView()
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

func test_default_arguments_do_not_interfere() {
  struct S {
    init(a: Int? = 42, b: String = "") {}
    func callAsFunction(_: () -> Void) -> S { S() }
  }

  _ = S { _ = 42 }
  _ = S(a: 42) { _ = 42 }
  _ = S(b: "") { _ = 42 }
}
