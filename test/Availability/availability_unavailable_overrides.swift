// RUN: %target-typecheck-verify-swift

func testAvailableOverrideOfUnavailableDecl() {
  class Base {
    @available(*, unavailable)
    func unavailableMethod() {}
    // expected-note@-1 2 {{'unavailableMethod()' has been explicitly marked unavailable here}}

    @available(*, unavailable)
    init(x: Int) {}
    // expected-note@-1 2 {{'init(x:)' has been explicitly marked unavailable here}}

    @available(*, unavailable)
    required init(requiredX: Int) {}
    // expected-note@-1 {{'init(requiredX:)' has been explicitly marked unavailable here}}

    @available(*, unavailable)
    subscript (i: Int) -> Int { return i }
    // expected-note@-1 2 {{'subscript(_:)' has been explicitly marked unavailable here}}

    @available(*, unavailable)
    var unavailableComputedProperty: Int {
      // expected-note@-1 2 {{'unavailableComputedProperty' has been explicitly marked unavailable here}}
      get { 0 }
      set {}
    }

    var computedPropertyWithUnavailableSet: Int {
      get { 0 }
      @available(*, unavailable)
      set {} // expected-note {{setter for 'computedPropertyWithUnavailableSet' has been explicitly marked unavailable here}}
    }
  }

  class Overrides: Base {
    override func unavailableMethod() {}
    // expected-error@-1 {{cannot override 'unavailableMethod' which has been marked unavailable}}
    // expected-note@-2 {{remove 'override' modifier to declare a new 'unavailableMethod'}}

    override init(x: Int) {}
    // expected-error@-1 {{cannot override 'init' which has been marked unavailable}}
    // expected-note@-2 {{remove 'override' modifier to declare a new 'init'}} {{5-14=}}

    // Required inits cannot be overridden using the `override` keyword.

    override subscript (i: Int) -> Int { return i }
    // expected-error@-1 {{cannot override 'subscript' which has been marked unavailable}}
    // expected-note@-2 {{remove 'override' modifier to declare a new 'subscript'}} {{5-14=}}

    override var unavailableComputedProperty: Int {
      // expected-error@-1 {{cannot override 'unavailableComputedProperty' which has been marked unavailable}}
      // expected-note@-2 {{remove 'override' modifier to declare a new 'unavailableComputedProperty'}} {{5-14=}}
      get { 1 }
      set {}
    }

    override var computedPropertyWithUnavailableSet: Int {
      get { 0 }
      set {} // expected-error {{cannot override setter for 'computedPropertyWithUnavailableSet' which has been marked unavailable}}
    }
  }

  // Redeclarations of the unavailable declarations (without `override`) are ok.
  class Shadows: Base {
    func unavailableMethod() {}
    init(x: Int) {}
    // NOTE: This is allowed by typechecking but it's actually uncallable and
    // will be rejected by flow sensitive analysis since it doesn't call
    // super.init(requiredX:).
    required init(requiredX: Int) {}
    subscript (i: Int) -> Int { return i }
    var unavailableComputedProperty: Int {
      get { 1 }
      set {}
    }
  }

  func use(base: Base, overrides: Overrides, shadows: Shadows) {
    base.unavailableMethod() // expected-error {{'unavailableMethod()' is unavailable}}
    overrides.unavailableMethod()
    shadows.unavailableMethod()

    _ = Base(x: 0) // expected-error {{'init(x:)' is unavailable}}
    _ = Overrides(x: 0)
    _ = Shadows(x: 0)

    _ = Base(requiredX: 0) // expected-error {{'init(requiredX:)' is unavailable}}
    _ = Shadows(requiredX: 0)

    _ = base[0] // expected-error {{'subscript(_:)' is unavailable}}
    _ = overrides[0]
    _ = shadows[0]

    _ = base.unavailableComputedProperty // expected-error {{'unavailableComputedProperty' is unavailable}}
    _ = overrides.unavailableComputedProperty
    _ = shadows.unavailableComputedProperty
  }
}

func testUnavailableOverrideOfAvailableDecl() {
  class Base {
    func availableMethod() {}
    init(x: Int) {}
    required init(requiredX: Int) {}
    subscript (i: Int) -> Int { return i }
    var availableComputedProperty: Int {
      get { 0 }
      set {}
    }
    var computedPropertyWithUnavailableSetterOverride: Int {
      get { 0 }
      set {}
    }
  }

  class Overrides: Base {
    @available(*, unavailable)
    override func availableMethod() {}

    @available(*, unavailable)
    override init(x: Int) {}

    @available(*, unavailable)
    required init(requiredX: Int) {}

    @available(*, unavailable)
    override subscript (i: Int) -> Int { return i }

    @available(*, unavailable)
    override var availableComputedProperty: Int {
      get { 1 }
      set {}
    }

    override var computedPropertyWithUnavailableSetterOverride: Int {
      get { 0 }
      @available(*, unavailable)
      set {}
    }
  }
}

func testUnavailableOverrideOfUnavailableDecl() {
  class Base {
    @available(*, unavailable)
    func unavailableMethod() {}

    @available(*, unavailable)
    init(x: Int) {}

    @available(*, unavailable)
    required init(requiredX: Int) {}

    @available(*, unavailable)
    subscript (i: Int) -> Int { return i }

    @available(*, unavailable)
    var unavailableComputedProperty: Int {
      get { 0 }
      set {}
    }

    var computedPropertyWithUnavailableSetterOverride: Int {
      get { 0 }
      @available(*, unavailable)
      set {}
    }
  }

  class Derived: Base {
    @available(*, unavailable)
    override func unavailableMethod() {}

    @available(*, unavailable)
    override init(x: Int) {}

    @available(*, unavailable)
    required init(requiredX: Int) {}

    @available(*, unavailable)
    override subscript (i: Int) -> Int { return i }

    @available(*, unavailable)
    override var unavailableComputedProperty: Int {
      get { 1 }
      set {}
    }

    override var computedPropertyWithUnavailableSetterOverride: Int {
      get { 0 }
      @available(*, unavailable)
      set {}
    }
  }
}

func testOverrideOfUnavailableDeclFromUnavailableDerivedType() {
  class Base {
    @available(*, unavailable)
    func availableMethod() {}

    @available(*, unavailable)
    init(x: Int) {}

    @available(*, unavailable)
    required init(requiredX: Int) {}

    @available(*, unavailable)
    subscript (i: Int) -> Int { return i }

    @available(*, unavailable)
    var availableComputedProperty: Int {
      get { 0 }
      set {}
    }
    var computedPropertyWithUnavailableSetterOverride: Int {
      get { 0 }
      @available(*, unavailable)
      set {}
    }
  }

  // Since Derived is unavailable, its OK that the overrides are not explicitly
  // marked unavailable.
  @available(*, unavailable)
  class Derived: Base {
    override func availableMethod() {}
    override init(x: Int) {}
    required init(requiredX: Int) {}
    override subscript (i: Int) -> Int { return i }
    override var availableComputedProperty: Int {
      get { 1 }
      set {}
    }
    override var computedPropertyWithUnavailableSetterOverride: Int {
      get { 0 }
      set {}
    }
  }
}


func testImplicitSuperInit() {
  // FIXME: The diagnostics for the implicit call to super.init() could be
  // relaxed since both initializers are unreachable and the developer cannot
  // wrap the call to super in a conditional compilation block.
  class Base {
    @available(*, unavailable)
    init() {} // expected-note {{'init()' has been explicitly marked unavailable here}}
  }

  class Derived: Base {
    @available(*, unavailable) // OK, matches base
    override init() {}
    // expected-error@-1 {{'init()' is unavailable}}
    // expected-note@-2 {{call to unavailable initializer 'init()' from superclass 'Base' occurs implicitly at the end of this initializer}}
  }
}
