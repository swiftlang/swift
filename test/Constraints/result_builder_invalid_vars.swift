// RUN: %target-typecheck-verify-swift

@resultBuilder
struct DummyBuilder { // expected-note 5 {{struct 'DummyBuilder' declared here}}
  static func buildBlock<T>(_ t: T) -> T {
    return t
  }
}

func dummy<T>(@DummyBuilder _: () -> T) {}

dummy {
  var computedVar: Int { return 123 } // expected-error {{cannot declare local computed variable in result builder}}
  ()
}

dummy {
  lazy var lazyVar: Int = 123 // expected-error {{cannot declare local lazy variable in result builder}}
  ()
}

dummy {
  var observedVar: Int = 123 { // expected-error {{cannot declare local observed variable in result builder}}
    didSet {}
  }

  ()
}

dummy {
  var observedVar: Int = 123 { // expected-error {{cannot declare local observed variable in result builder}}
    willSet {}
  }

  ()
}

@propertyWrapper struct Wrapper {
  var wrappedValue: Int
}

dummy {
  @Wrapper var wrappedVar: Int = 123 // expected-error {{cannot declare local wrapped variable in result builder}}
  ()
}

dummy {
  @resultBuilder var attributedVar: Int = 123 // expected-error {{@resultBuilder' attribute cannot be applied to this declaration}}
  // expected-warning@-1 {{variable 'attributedVar' was never used}}
  // expected-note@-2 {{consider replacing with '_' or removing it}}
  ()
}
