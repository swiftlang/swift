// RUN: %target-typecheck-verify-swift


struct Outer {
  struct Inner {
    struct Foo<T> {
      static func id(_ v: T) -> T { v }
    }
  }
}

let _: Outer = .Inner.Foo<Outer>.id(.init())

