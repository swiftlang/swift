// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/56212

enum E {
case foo(String)
}

struct Test {
  var bar: E?
}

struct S {
  func evaluate(_: Test) -> [Test] {
    return []
  }

  func test(set: Set<String>)  {
    let flattened = set.flatMap { element in
      evaluate(Test(bar: .foo(element)))
    }

    let _: [Test] = flattened // Ok (find .`bar` after unwrap)
  }
}
