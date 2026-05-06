// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix swift5- %s -swift-version 5
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix swift6- %s -swift-version 6


class NonOptToOpt {
  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init() {}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init?() {}
}

_ = NonOptToOpt() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}

class NonOptToOptReversed {
  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init?() {}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init() {}
}

_ = NonOptToOptReversed() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}


class OptToNonOpt {
  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init!() {}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init() {}
}

_ = OptToNonOpt() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}

class OptToNonOptReversed {
  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init() {}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init!() {}
}

_ = OptToNonOptReversed() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}


class NoChange {
  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init() {} // expected-note {{'init()' previously declared here}}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init() {} // expected-error {{invalid redeclaration of 'init()'}}
}

class NoChangeReversed {
  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init() {} // expected-note {{'init()' previously declared here}}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init() {} // expected-error {{invalid redeclaration of 'init()'}}
}

class OptToOpt {
  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init!() {} // expected-note {{'init()' previously declared here}}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init?() {} // expected-error {{invalid redeclaration of 'init()'}}
}

class OptToOptReversed {
  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init?() {} // expected-note {{'init()' previously declared here}}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init!() {} // expected-error {{invalid redeclaration of 'init()'}}
}

class ThreeWayA {
  @available(swift, obsoleted: 6.0)
  public init() {}

  @available(swift, introduced: 6.0, obsoleted: 7.0)
  public init?() {}

  @available(swift, introduced: 7.0)
  public init() throws {}
}

class ThreeWayB {
  @available(swift, obsoleted: 6.0)
  public init() {}

  @available(swift, introduced: 7.0)
  public init() throws {}

  @available(swift, introduced: 6.0, obsoleted: 7.0)
  public init?() {}
}

class ThreeWayC {
  @available(swift, introduced: 7.0)
  public init() throws {}

  @available(swift, obsoleted: 6.0)
  public init() {}

  @available(swift, introduced: 6.0, obsoleted: 7.0)
  public init?() {}
}

class ThreeWayD {
  @available(swift, introduced: 7.0)
  public init() throws {}

  @available(swift, introduced: 6.0, obsoleted: 7.0)
  public init?() {}

  @available(swift, obsoleted: 6.0)
  public init() {}
}

class ThreeWayE {
  @available(swift, introduced: 6.0, obsoleted: 7.0)
  public init?() {}

  @available(swift, introduced: 7.0)
  public init() throws {}

  @available(swift, obsoleted: 6.0)
  public init() {}
}

class ThreeWayF {
  @available(swift, introduced: 6.0, obsoleted: 7.0)
  public init?() {}

  @available(swift, obsoleted: 6.0)
  public init() {}

  @available(swift, introduced: 7.0)
  public init() throws {}
}

class DisjointThreeWay {
  @available(swift, obsoleted: 6.0)
  public init() {}

  @available(swift, introduced: 6.1, obsoleted: 7.0)
  public init?() {}

  @available(swift, introduced: 7.1)
  public init() throws {}
}

class OverlappingVersions {
  @available(swift, obsoleted: 7.0)
  public init(a: ()) {} // expected-note {{'init(a:)' previously declared here}}

  @available(swift 6.0)
  public init?(a: ()) {} // expected-error {{invalid redeclaration of 'init(a:)'}}

  @available(swift 6.0)
  public init?(b: ()) {} // expected-note {{'init(b:)' previously declared here}}

  @available(swift, obsoleted: 6.1)
  public init(b: ()) {} // expected-error {{invalid redeclaration of 'init(b:)'}}

  public init(c: ()) {} // expected-note {{'init(c:)' previously declared here}}

  @available(swift 6.0)
  public init?(c: ()) {} // expected-error {{invalid redeclaration of 'init(c:)'}}

  @available(swift 6.0)
  public init(c2: ()) {} // expected-note {{'init(c2:)' previously declared here}}

  public init?(c2: ()) {} // expected-error {{invalid redeclaration of 'init(c2:)'}}

  @available(swift, obsoleted: 6.0)
  public init(d: ()) {} // expected-note {{'init(d:)' previously declared here}}

  public init?(d: ()) {} // expected-error {{invalid redeclaration of 'init(d:)'}}

  public init(d2: ()) {} // expected-note {{'init(d2:)' previously declared here}}

  @available(swift, obsoleted: 6.0)
  public init?(d2: ()) {} // expected-error {{invalid redeclaration of 'init(d2:)'}}

  @available(swift, obsoleted: 6.0)
  public init(e: ()) {}

  @available(swift 6.0)
  public init?(e: ()) {} // expected-note {{'init(e:)' previously declared here}}

  @available(swift 6.0)
  public init!(e: ()) {} // expected-error {{invalid redeclaration of 'init(e:)'}}

  @available(swift, obsoleted: 6.0)
  public init(f: ()) {} // expected-note {{'init(f:)' previously declared here}}

  @available(swift 6.0)
  public init?(f: ()) {}

  @available(swift, obsoleted: 6.0)
  public init!(f: ()) {} // expected-error {{invalid redeclaration of 'init(f:)'}}
}


class NonThrowingToThrowing {
  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init() {}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init() throws {}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public static func foo() {}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public static func foo() throws {}

  static func test() throws {
    _ = NonThrowingToThrowing() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift6-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift6-note@-2 {{did you mean to use 'try'?}}
    // expected-swift6-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift6-note@-4 {{did you mean to disable error propagation?}}
    _ = NonThrowingToThrowing.foo() // expected-warning {{using '_' to ignore the result of a Void-returning function is redundant}} expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift6-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift6-note@-2 {{did you mean to use 'try'?}}
    // expected-swift6-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift6-note@-4 {{did you mean to disable error propagation?}}
  }

  @available(swift, obsoleted: 6.0)
  func testObsoleted() throws {
    _ = NonThrowingToThrowing() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift6-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift6-note@-2 {{did you mean to use 'try'?}}
    // expected-swift6-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift6-note@-4 {{did you mean to disable error propagation?}}
    _ = NonThrowingToThrowing.foo() // expected-warning {{using '_' to ignore the result of a Void-returning function is redundant}} expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift6-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift6-note@-2 {{did you mean to use 'try'?}}
    // expected-swift6-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift6-note@-4 {{did you mean to disable error propagation?}}
  }

}

class NonThrowingToThrowingReversed {
  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init() throws {}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init() {}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public static func foo() throws {}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public static func foo() {}

  static func test() throws {
    _ = NonThrowingToThrowingReversed() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift6-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift6-note@-2 {{did you mean to use 'try'?}}
    // expected-swift6-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift6-note@-4 {{did you mean to disable error propagation?}}
    _ = NonThrowingToThrowingReversed.foo() // expected-warning {{using '_' to ignore the result of a Void-returning function is redundant}} expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift6-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift6-note@-2 {{did you mean to use 'try'?}}
    // expected-swift6-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift6-note@-4 {{did you mean to disable error propagation?}}
  }

  @available(swift, obsoleted: 6.0)
  func testObsoleted() throws {
    _ = NonThrowingToThrowingReversed() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift6-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift6-note@-2 {{did you mean to use 'try'?}}
    // expected-swift6-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift6-note@-4 {{did you mean to disable error propagation?}}
    _ = NonThrowingToThrowingReversed.foo() // expected-warning {{using '_' to ignore the result of a Void-returning function is redundant}} expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift6-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift6-note@-2 {{did you mean to use 'try'?}}
    // expected-swift6-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift6-note@-4 {{did you mean to disable error propagation?}}
  }
}

class ThrowingToNonThrowing {
  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init() throws {}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init() {}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public static func foo() throws {}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public static func foo() {}

  static func test() throws {
    _ = ThrowingToNonThrowing() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift5-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift5-note@-2 {{did you mean to use 'try'?}}
    // expected-swift5-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift5-note@-4 {{did you mean to disable error propagation?}}
    _ = ThrowingToNonThrowing.foo() // expected-warning {{using '_' to ignore the result of a Void-returning function is redundant}} expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift5-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift5-note@-2 {{did you mean to use 'try'?}}
    // expected-swift5-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift5-note@-4 {{did you mean to disable error propagation?}}
  }

  @available(swift, obsoleted: 6.0)
  func testObsoleted() throws {
    _ = ThrowingToNonThrowing() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift5-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift5-note@-2 {{did you mean to use 'try'?}}
    // expected-swift5-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift5-note@-4 {{did you mean to disable error propagation?}}
    _ = ThrowingToNonThrowing.foo() // expected-warning {{using '_' to ignore the result of a Void-returning function is redundant}} expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift5-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift5-note@-2 {{did you mean to use 'try'?}}
    // expected-swift5-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift5-note@-4 {{did you mean to disable error propagation?}}
  }
}

class ThrowingToNonThrowingReversed {
  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public init() {}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public init() throws {}

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public static func foo() {}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public static func foo() throws {}

  static func test() throws {
    _ = ThrowingToNonThrowingReversed() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift5-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift5-note@-2 {{did you mean to use 'try'?}}
    // expected-swift5-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift5-note@-4 {{did you mean to disable error propagation?}}
    _ = ThrowingToNonThrowingReversed.foo() // expected-warning {{using '_' to ignore the result of a Void-returning function is redundant}} expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift5-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift5-note@-2 {{did you mean to use 'try'?}}
    // expected-swift5-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift5-note@-4 {{did you mean to disable error propagation?}}
  }

  @available(swift, obsoleted: 6.0)
  func testObsoleted() throws {
    _ = ThrowingToNonThrowingReversed() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift5-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift5-note@-2 {{did you mean to use 'try'?}}
    // expected-swift5-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift5-note@-4 {{did you mean to disable error propagation?}}
    _ = ThrowingToNonThrowingReversed.foo() // expected-warning {{using '_' to ignore the result of a Void-returning function is redundant}} expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
    // expected-swift5-error@-1 {{call can throw but is not marked with 'try'}}
    // expected-swift5-note@-2 {{did you mean to use 'try'?}}
    // expected-swift5-note@-3 {{did you mean to handle error as optional value?}}
    // expected-swift5-note@-4 {{did you mean to disable error propagation?}}
  }
}

class ChangePropertyType {

  // We don't allow this for stored properties.

  @available(swift 6.0) // expected-swift5-error {{stored properties cannot be marked unavailable with '@available'}}
  @available(*, deprecated, message: "yes 6.0")
  public var stored: Int16 = 0 // expected-note {{'stored' previously declared here}}

  @available(swift, obsoleted: 6.0) // expected-swift6-error {{stored properties cannot be marked unavailable with '@available'}}
  @available(*, deprecated, message: "not 6.0")
  public var stored: Int8 = 0 // expected-error {{invalid redeclaration of 'stored'}}

  // OK for computed properties.

  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public var computed: Int16 { get { } set { } }

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public var computed: Int8 { get { } set { } }

  func test() {
    _ = computed // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
  }

  @available(swift, obsoleted: 6.0)
  func testObsoleted() {
    _ = computed // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
  }
}

class ExtendMe { }

extension ExtendMe {
  @available(swift 6.0)
  @available(*, deprecated, message: "yes 6.0")
  public func bar() -> Int16 {}

  @available(swift, obsoleted: 6.0)
  @available(*, deprecated, message: "not 6.0")
  public func bar() -> Int8 {}

  func test() {
    _ = bar() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
  }

  @available(swift, obsoleted: 6.0)
  func testObsoleted() {
    _ = bar() // expected-swift5-warning {{not 6.0}} expected-swift6-warning {{yes 6.0}}
  }
}
