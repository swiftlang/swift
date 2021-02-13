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

func freeFloatingRethrowingFromExistential(_ r: RethrowingProtocol) rethrows {
  // expected-error@-1{{'rethrows' function must take a throwing function argument}}
}

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

func freeFloatingInheritedRethrowingFunctionFromExistential(_ r: InheritsRethrowing) rethrows {
  // expected-error@-1{{'rethrows' function must take a throwing function argument}}
}

func closureAndRethrowing<R: RethrowingProtocol>(_ r: R, _ closure: () throws -> Void) rethrows { }

closureAndRethrowing(NonThrows()) { }
try closureAndRethrowing(NonThrows()) { } // expected-warning{{no calls to throwing functions occur within 'try' expression}}
try closureAndRethrowing(Throws()) { }
try closureAndRethrowing(NonThrows()) { () throws -> Void in }

// Make sure we handle the case where both the 'self' parameter and a closure
// argument are rethrows sources.
extension RethrowingProtocol {
  func selfRethrowing() rethrows { }
  func closureAndSelfRethrowing(_ closure: () throws -> Void) rethrows { }
}

NonThrows().selfRethrowing()
try Throws().selfRethrowing()

NonThrows().closureAndSelfRethrowing { }
try NonThrows().closureAndSelfRethrowing { () throws -> Void in }

try Throws().closureAndSelfRethrowing { }
try Throws().closureAndSelfRethrowing { () throws -> Void in }

// Soundness hole
@rethrows protocol ThrowsClosure {
  func doIt() throws
  func doIt(_: () throws -> ()) throws
}

struct ThrowsClosureWitness : ThrowsClosure {
  func doIt() {}
  func doIt(_: () throws -> ()) throws {}
}

func rethrowsWithThrowsClosure<T : ThrowsClosure>(_ t: T) rethrows {
  try t.doIt() {}
}

try rethrowsWithThrowsClosure(ThrowsClosureWitness())

@rethrows protocol RethrowsClosure {
  func doIt() throws
  func doIt(_: () throws -> ()) rethrows
}

struct RethrowsClosureWitness : RethrowsClosure {
  func doIt() {}
  func doIt(_: () throws -> ()) rethrows {}
}

func rethrowsWithRethrowsClosure<T : RethrowsClosure>(_ t: T) rethrows {
  try t.doIt() {}
}

rethrowsWithRethrowsClosure(RethrowsClosureWitness())

// Empty protocol
@rethrows protocol Empty {}
struct EmptyWitness : Empty {}

func takesEmpty<T : Empty>(_: T) rethrows {}

takesEmpty(EmptyWitness())

// FIXME: Fix this soundness hole

// Note: SimpleThrowsClosure is not @rethrows
protocol SimpleThrowsClosure {
  func doIt(_: () throws -> ()) rethrows
}

struct ConformsToSimpleThrowsClosure<T : RethrowingProtocol> : SimpleThrowsClosure {
  let t: T

  // This cannot witness SimpleThrowsClosure.doIt(), because the
  // T : RethrowingProtocol conformance is a source here, but that
  // is not captured in the protocol's requirement signature.
  func doIt(_: () throws -> ()) rethrows {
    try t.source()
  }
}

func soundnessHole<T : SimpleThrowsClosure>(_ t: T) {
  t.doIt {}
}

// This actually can throw...
soundnessHole(ConformsToSimpleThrowsClosure(t: Throws()))

// Test deeply-nested associated conformances
@rethrows protocol First {
  associatedtype A : Second
}

@rethrows protocol Second {
  associatedtype B : Third
}

@rethrows protocol Third {
  func f() throws
}

struct FirstWitness : First {
  typealias A = SecondWitness
}

struct SecondWitness : Second {
  typealias B = ThirdWitness
}

struct ThirdWitness : Third {
  func f() {}
}

func takesFirst<T : First>(_: T) rethrows {}

takesFirst(FirstWitness())

// Crash with enum case
@rethrows protocol WitnessedByEnumCase {
  static func foo(_: Int) throws -> Self
}

enum MyEnum : WitnessedByEnumCase {
  case foo(Int)
  case bar
}

func takesWitnessedByEnumCase<T : WitnessedByEnumCase>(_: T) rethrows {}

takesWitnessedByEnumCase(MyEnum.bar)