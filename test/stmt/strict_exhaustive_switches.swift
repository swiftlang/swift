// RUN: split-file %s %t

// Use fragile enums
// RUN: %target-swift-frontend %t/Enums.swift -emit-module -module-name enums -o %t/enums.swiftmodule
// RUN: %target-swift-frontend    -I %t %t/NewDiagnostics.swift %t/PreventRegressions.swift %t/WithError.swift -swift-version 6 -Wwarning StrictExhaustiveSwitches -verify -typecheck
// RUN: %target-swift-emit-silgen -I %t %t/NewDiagnostics.swift %t/PreventRegressions.swift %t/SILVerify.swift -swift-version 6 -Wwarning StrictExhaustiveSwitches -verify -o /dev/null

// Repeat with resilient enums
// RUN: %target-swift-frontend %t/Enums.swift -emit-module -module-name enums -o %t/enums.swiftmodule -enable-library-evolution
// RUN: %target-swift-frontend    -I %t %t/NewWithResilience.swift %t/PreventRegsResilience.swift %t/ErrResilience.swift -swift-version 6 -Wwarning StrictExhaustiveSwitches -verify -typecheck
// RUN: %target-swift-emit-silgen -I %t %t/NewWithResilience.swift %t/PreventRegsResilience.swift %t/SILResilience.swift -swift-version 6 -Wwarning StrictExhaustiveSwitches -verify -o /dev/null

//--- Enums.swift

public enum ReasonableEnum {
  case zero
  case one
}

public enum OverlyLargeSpaceEnum {
  case case0
  case case1
  case case2
  case case3
  case case4
  case case5
  case case6
  case case7
  case case8
  case case9
  case case10
  case case11
}

//--- NewDiagnostics.swift

import enums

func testMissingKnownCases(enum o: OverlyLargeSpaceEnum) -> Bool {
  switch o {
    // expected-note@-1 {{add missing cases}}
    // expected-note@-2 11 {{add missing case}}
    case .case0: return true
    default: return false // expected-warning {{switch can be made exhaustive without use of 'default'; replace with known cases}}
  }
}

func testMissingWildcards(e1: ReasonableEnum, e2: ReasonableEnum) -> Bool {
  switch (e1, e2) { // expected-note {{add missing cases}}
    // expected-note@-1 {{add missing case: '(.one, _)'}}
    // expected-note@-2 {{add missing case: '(_, .one)'}}
    case (.zero, .zero): return true
    default: return false // expected-warning {{switch can be made exhaustive without use of 'default'; replace with known cases}}
  }
}

func testMissingWildcards(o1: OverlyLargeSpaceEnum, o2: OverlyLargeSpaceEnum) -> Bool {
  switch (o1, o2) { // expected-note {{add missing case: '(.case11, _)'}}
  case (.case0, _): return true
  case (.case1, _): return true
  case (.case2, _): return true
  case (.case3, _): return true
  case (.case4, _): return true
  case (.case5, _): return true
  case (.case6, _): return true
  case (.case7, _): return true
  case (.case8, _): return true
  case (.case9, _): return true
  case (.case10, _): return true
  default: return false // expected-warning {{switch can be made exhaustive without use of 'default'; replace with known cases}}
  }
}


//--- NewWithResilience.swift

import enums

func testMissingKnownCases(enum o: OverlyLargeSpaceEnum) -> Bool {
  switch o {
    // expected-note@-1 {{add missing cases}}
    // expected-note@-2 11 {{add missing case}}
    case .case0: return true
    default: return false // expected-warning {{switch can be made exhaustive without use of 'default'; replace with known cases}}
  }
}

func testMissingWildcards(e1: ReasonableEnum, e2: ReasonableEnum) -> Bool {
  switch (e1, e2) { // expected-note {{add missing cases}}
    // expected-note@-1 {{add missing case: '(.one, _)'}}
    // expected-note@-2 {{add missing case: '(_, .one)'}}
    // expected-note@-3 2 {{add missing case: '(_, _)'}}
    case (.zero, .zero): return true
    default: return false // expected-warning {{switch can be made exhaustive without use of 'default'; replace with known cases}}
  }
}

func testMissingWildcards(o1: OverlyLargeSpaceEnum, o2: OverlyLargeSpaceEnum) -> Bool {
  switch (o1, o2) { // expected-note {{add missing cases}}
  // expected-note@-1 {{add missing case: '(.case11, _)'}}
  // expected-note@-2 {{add missing case: '(_, _)'}}
  case (.case0, _): return true
  case (.case1, _): return true
  case (.case2, _): return true
  case (.case3, _): return true
  case (.case4, _): return true
  case (.case5, _): return true
  case (.case6, _): return true
  case (.case7, _): return true
  case (.case8, _): return true
  case (.case9, _): return true
  case (.case10, _): return true
  default: return false // expected-warning {{switch can be made exhaustive without use of 'default'; replace with known cases}}
  }
}

//--- SILVerify.swift

import enums

func testExhaustiveWithDefault(enum e: ReasonableEnum) -> Bool {
  switch e {
    case .zero, .one: return true
    default: return false // expected-warning {{default will never be executed}}
  }
}

func testWildcardWithDefault(enum e: ReasonableEnum) -> Bool {
  switch e {
    case _: // expected-warning {{a 'default' case is equivalent to a catch-all case; remove one}}
      return true
    default: // expected-warning {{default will never be executed}}
      return false
  }
}

func testTupleWildcardWithDefault(e1: ReasonableEnum, e2: ReasonableEnum) -> Bool {
  switch (e1, e2) {
  case (_, _): // expected-warning {{a 'default' case is equivalent to a catch-all case; remove one}}
    return true
  default: // expected-warning {{default will never be executed}}
    return false
  }
}

func testParensWildcardWithDefault(e1: ReasonableEnum) -> Bool {
  switch (e1) {
  case ((_)): // expected-warning {{a 'default' case is equivalent to a catch-all case; remove one}}
      return true
    default: // expected-warning {{default will never be executed}}
      return false
  }
}

func testGuardedWildcardWithDefault(enum e: ReasonableEnum) -> Bool {
  switch e {
    case .zero, .one:
      return true
    case _ where e == .one:
      return true
    default:
      return false
  }
}

//--- SILResilience.swift

import enums

func testExhaustiveWithDefault(enum e: ReasonableEnum) -> Bool {
  switch e { // expected-error {{switch covers known cases, but 'ReasonableEnum' may have additional unknown values, 'default' [or 'case _'] will only be executed for unknown values [possibly added in future versions]; replace with "@unknown default"}} {{7:3-3=@unknown }}
  case .zero, .one: return true
  default: return false
  }
}

func testWildcardWithDefault(enum e: ReasonableEnum) -> Bool {
  switch e {
  case _: // expected-warning {{a 'default' case is equivalent to a catch-all case; remove one}}
    return true
  default:
    return false
  }
}

func testGuardedWildcardWithDefault(enum e: ReasonableEnum) -> Bool {
  switch e { // expected-error {{switch covers known cases, but 'ReasonableEnum' may have additional unknown values, 'default' [or 'case _'] will only be executed for unknown values [possibly added in future versions]; replace with "@unknown default"}}
    case .zero, .one:
      return true
    case _ where e == .one:
      return true
    default:
      return false
  }
}

func testTupleWildcardWithDefault(e1: ReasonableEnum, e2: ReasonableEnum) -> Bool {
  switch (e1, e2) {
  case (_, _): // expected-warning {{a 'default' case is equivalent to a catch-all case; remove one}}
    return true
  default:
    return false
  }
}

func testParensWildcardWithDefault(e1: ReasonableEnum) -> Bool {
  switch (e1) {
  case ((_)): // expected-warning {{a 'default' case is equivalent to a catch-all case; remove one}}
    return true
  default:
    return false
  }
}

//--- PreventRegressions.swift

import enums

func testWellFormedExhaustive(enum e: ReasonableEnum) -> Bool {
  switch e {
    case .zero, .one: return true
  }
}

func testExhaustiveOpen(value: Int) -> Bool {
  switch value {
  case 1: return true
  case 2...10: return true
  default: return false
  }
}

func testExhaustiveWithUnknownDefault(e: ReasonableEnum) -> Bool {
  switch e {
  case .zero, .one: return true
  @unknown default: return false
  }
}

func testNonExhaustiveWithUnknownDefault(e: ReasonableEnum) -> Bool {
  switch ReasonableEnum.zero { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '.one'}}
  case .zero: return true
  @unknown default: return false
  }
}

func testExhaustiveTupleWithUnknownDefault(e1: ReasonableEnum, e2: ReasonableEnum) -> Bool {
  switch (ReasonableEnum.zero, ReasonableEnum.one) {
  case (.zero, _), (.one, _): return true
  @unknown default: return false
  }
}

func nonExhaustiveTupleWithUnknownDeafult(e1: ReasonableEnum, e2: ReasonableEnum) -> Bool {
  switch (ReasonableEnum.zero, ReasonableEnum.one) { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.one, _)'}}
  case (.zero, _): return true
  @unknown default: return false
  }
}


//--- PreventRegsResilience.swift

import enums

func testSuggestUnkonwnDefaultForResilientEnum(enum e: ReasonableEnum) -> Bool {
  switch e { // expected-error {{switch covers known cases, but 'ReasonableEnum' may have additional unknown values}}
  // expected-note@-1 {{handle unknown values using "@unknown default"}}
  case .zero, .one: return true
  }
}

func testExhaustiveOpen(value: Int) -> Bool {
  switch value {
  case 1: return true
  case 2...10: return true
  default: return false
  }
}

func testExhaustiveWithUnknownDefault(e: ReasonableEnum) -> Bool {
  switch e {
  case .zero, .one: return true
  @unknown default: return false
  }
}

func testNonExhaustiveWithUnknownDefault(e: ReasonableEnum) -> Bool {
  switch ReasonableEnum.zero { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '.one'}}
  case .zero: return true
  @unknown default: return false
  }
}

func testExhaustiveTupleWithUnknownDefault(e1: ReasonableEnum, e2: ReasonableEnum) -> Bool {
  switch (ReasonableEnum.zero, ReasonableEnum.one) {
  case (.zero, _), (.one, _): return true
  @unknown default: return false
  }
}

func nonExhaustiveTupleWithUnknownDeafult(e1: ReasonableEnum, e2: ReasonableEnum) -> Bool {
  switch (ReasonableEnum.zero, ReasonableEnum.one) { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.one, _)'}}
  case (.zero, _): return true
  @unknown default: return false
  }
}

//--- WithError.swift

// split file to verify type-checking but can't get to sil with error
func testNonExhaustiveOpen(value: Int) -> Bool {
  switch value { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{add a default clause}}
  case 1: return true
  case 2...10: return true
  }
}


//--- ErrResilience.swift

import enums

func testMissingWildcardsNoDefault(e1: ReasonableEnum, e2: ReasonableEnum) -> Bool {
  switch (e1, e2) { // expected-error {{switch must be exhaustive}}
    // expected-note@-1 {{add missing cases}}
    // expected-note@-2 {{add missing case: '(.one, _)'}}
    // expected-note@-3 {{add missing case: '(_, .one)'}}
    // expected-note@-4 2 {{add missing case: '(_, _)'}}
    case (.zero, .zero): return true
  }
}
