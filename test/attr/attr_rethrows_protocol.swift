// RUN: %target-typecheck-verify-swift

@rethrows
protocol RethrowingProtocol {
  func source() throws
}

struct Rethrows<Source: RethrowingProtocol>: RethrowingProtocol {
  var other: Source
  func source() rethrows { }
}

struct Throws: RethrowingProtocol {
  func source() throws { }
}

struct ThrowsWithSource<Source: RethrowingProtocol>: RethrowingProtocol {
  var other: Source
  func source() throws { }
}

struct NonThrows: RethrowingProtocol {
  func source() { }
}

struct NonThrowsWithSource<Source: RethrowingProtocol>: RethrowingProtocol {
  var other: Source
  func source() { }
}

protocol InvalidRethrowingProtocol {
  func source() throws
}

struct InvalidRethrows : InvalidRethrowingProtocol {
  func source() rethrows { }
  // expected-error@-1{{'rethrows' function must take a throwing function argument}}
}

func freeFloatingRethrowing<R: RethrowingProtocol>(_ r: R) rethrows { }

func freeFloatingRethrowingFromExistential(_ r: RethrowingProtocol) rethrows { }

func invalidFreeFloatingRethrows() rethrows {
  // expected-error@-1{{'rethrows' function must take a throwing function argument}}
}

let rethrowingFromThrows = Rethrows(other: Throws())
try rethrowingFromThrows.source()

@rethrows
protocol HasAssociatedRethrowerWithEnclosedRethrow {
  associatedtype Rethrower: RethrowingProtocol

  func source() throws
}

@rethrows
protocol HasAssociatedRethrower {
  associatedtype Rethrower: RethrowingProtocol

  func makeRethrower() -> Rethrower
}

func freeFloatingRethrowing<R: HasAssociatedRethrower>(_ r: R) rethrows { }

@rethrows
protocol InheritsRethrowing: RethrowingProtocol {}

func freeFloatingInheritedRethrowingFunction<I: InheritsRethrowing>(_ r: I) rethrows { }
func freeFloatingInheritedRethrowingFunctionFromExistential(_ r: InheritsRethrowing) rethrows { }

func closureAndRethrowing<R: RethrowingProtocol>(_ r: R, _ closure: () throws -> Void) rethrows { }

closureAndRethrowing(NonThrows()) { }
try closureAndRethrowing(NonThrows()) { } // expected-warning{{no calls to throwing functions occur within 'try' expression}}
try closureAndRethrowing(Throws()) { }
try closureAndRethrowing(NonThrows()) { () throws -> Void in }
