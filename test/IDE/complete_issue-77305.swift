// RUN: %batch-code-completion

// https://github.com/swiftlang/swift/issues/77305

struct S {
  var x: Int
}

func withFoo(_ x: (S) -> Void) {}

withFoo { foo in
  func bar() {
    foo.#^FN_IN_CLOSURE^#
    // FN_IN_CLOSURE: Decl[InstanceVar]/CurrNominal: x[#Int#]; name=x
  }
}

withFoo { x in
  _ = { y in
    func bar() {
      _ = { z in
        func baz() {
          func qux() {
            z.#^VERY_NESTED_FN_IN_CLOSURE^#
            // VERY_NESTED_FN_IN_CLOSURE: Decl[InstanceVar]/CurrNominal: x[#Int#]; name=x
          }
        }
      }(y)
    }
  }(x)
}
