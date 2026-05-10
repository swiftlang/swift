// RUN: %target-typecheck-verify-swift

@resultBuilder
struct DummyBuilder {
  static func buildBlock<T>(_ t: T) -> T {
    return t
  }
}

func dummy<T>(@DummyBuilder _: () -> T) {}

dummy {
  var computedVar: Int { return 123 }
  ()
}

dummy {
  lazy var lazyVar: Int = 123
  ()
}

dummy {
  var observedVar: Int = 123 {
    // expected-warning@-1 {{variable 'observedVar' was never used; consider replacing with '_' or removing it}}
    didSet {}
  }

  ()
}

dummy {
  var observedVar: Int = 123 {
    // expected-warning@-1 {{variable 'observedVar' was never used; consider replacing with '_' or removing it}}
    willSet {}
  }

  ()
}

@propertyWrapper struct Wrapper {
  var wrappedValue: Int
}

dummy {
  @Wrapper var wrappedVar: Int = 123
  ()
}

dummy {
  @resultBuilder var attributedVar: Int = 123
  // expected-error@-1 {{'@resultBuilder' attribute cannot be applied to this declaration}}
  // expected-warning@-2 {{variable 'attributedVar' was never used; consider replacing with '_' or removing it}}
  ()
}
