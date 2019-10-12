// RUN: %target-swift-frontend -typecheck -primary-file %s -verify -module-name main

func compute() -> Bool {
  return true
}
func thrower() throws -> Bool {
  return true
}

/*=====================================*/
/*              Top-level              */
/*=====================================*/

lazy var topLevelLazy1 = compute()

/*=====================================*/
/*                Local                */
/*=====================================*/

func testLocalLazyUsage() -> Bool {
  lazy var lazy1 = compute()

  if lazy1 {
    lazy var lazy2 = compute()

    guard lazy2 else {
      lazy var lazy3 = compute()
      return lazy3
    }
    for _ in [lazy1, lazy2] {
      lazy var lazy4 = compute()
      lazy1 = lazy4
    }
    _ = [lazy1, lazy2].map { _ -> Bool in
      lazy var lazy5 = compute()
      return lazy5
    }
  }
  #if DEBUG
  lazy var lazy6 = compute()
  return lazy6
  #else
  return lazy1
  #endif
}

class LocalLazyWrapperClass {
  func compute() -> Bool { return true }

  func testLocalLazyInitializers() {
    lazy let a = 0
    // expected-error@-1 {{'lazy' cannot be used on a let}}

    lazy var b: Int
    // expected-error@-1 {{lazy properties must have an initializer}}

    lazy var (c1, c2) = (0, 1)
    // expected-error@-1 2 {{'lazy' cannot destructure an initializer}}

    lazy var d: Int = 0 { willSet {} }
    // expected-error@-1 {{lazy properties must not have observers}}
    // expected-warning@-2 {{variable 'd' was never used}}

    lazy var e = { 0 }()

    lazy var f = self.compute()

    lazy var g = { [unowned self] in return self.compute }()

    lazy var h = { [weak self] in return self?.compute }()
  }
}
