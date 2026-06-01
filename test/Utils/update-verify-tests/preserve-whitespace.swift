// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func wsAtBumpToTwo() {
  //  expected-error  @+1  {{cannot find 'unboundAlpha' in scope}}
  unboundAlpha = 1; unboundAlpha = 1
}

func wsCountBumpToThree() {
  // expected-error@+1   {{cannot find 'unboundBeta' in scope}}
  unboundBeta = 1; unboundBeta = 1; unboundBeta = 1
}

func allFourSlotsDropToOne() {
  //   expected-error  @+1   2  {{cannot find 'unboundGamma' in scope}}
  unboundGamma = 1
}

func synthesizedSmushPrevention() {
  unboundDelta = 1; unboundDelta = 1
}

func explicitOneBumpToTwo() {
  // expected-error@+1 1{{cannot find 'unboundEpsilon' in scope}}
  unboundEpsilon = 1; unboundEpsilon = 1
}

func explicitOnePreservedOnMessageChange() {
  // expected-error@+1 1{{wrong message text}}
  unboundZeta = 1
}

func explicitOneRoundTripWhenOtherUpdates() {
  func dupOmicron() {} // expected-note 1{{'dupOmicron()' previously declared here}}
  func dupOmicron() {} // expected-error{{wrong text}}
}

func bracesWsPreservedOnBump() {
  // expected-error@+1 1 {{cannot find 'unboundEta' in scope}}
  unboundEta = 1; unboundEta = 1
}

func multiBlockTailPreserved() {
  unboundTheta = 1 // expected-error{{cannot find 'unboundTheta' in scope}}{{none}}
  unboundIota = 1 // expected-error{{wrong text}}
}

func multiBlockTailSpaced() {
  unboundMu = 1 // expected-error{{cannot find 'unboundMu' in scope}}  {{none}}
  unboundNu = 1 // expected-error{{wrong text}}
}

@available(*, deprecated, message: "see \\junk")
func deprecatedKappa() {}

func escapesInMessagePreserved() {
  deprecatedKappa() // expected-warning{{'deprecatedKappa()' is deprecated: see \\junk}}
  unboundLambda = 1 // expected-error{{wrong text}}
}

func messageChangeWithBackwardTarget() {
  unboundOmega = 1
  // expected-error @-1 {{wrong text}}
}

func colOnlyTarget() {
  unboundPi = 1 // expected-error@:3{{wrong text}}
}

//--- test.swift.expected
func wsAtBumpToTwo() {
  //  expected-error  @+1  2{{cannot find 'unboundAlpha' in scope}}
  unboundAlpha = 1; unboundAlpha = 1
}

func wsCountBumpToThree() {
  // expected-error@+1   3{{cannot find 'unboundBeta' in scope}}
  unboundBeta = 1; unboundBeta = 1; unboundBeta = 1
}

func allFourSlotsDropToOne() {
  //   expected-error  @+1     {{cannot find 'unboundGamma' in scope}}
  unboundGamma = 1
}

func synthesizedSmushPrevention() {
  // expected-error@+1 2{{cannot find 'unboundDelta' in scope}}
  unboundDelta = 1; unboundDelta = 1
}

func explicitOneBumpToTwo() {
  // expected-error@+1 2{{cannot find 'unboundEpsilon' in scope}}
  unboundEpsilon = 1; unboundEpsilon = 1
}

func explicitOnePreservedOnMessageChange() {
  // expected-error@+1 1{{cannot find 'unboundZeta' in scope}}
  unboundZeta = 1
}

func explicitOneRoundTripWhenOtherUpdates() {
  func dupOmicron() {} // expected-note 1{{'dupOmicron()' previously declared here}}
  func dupOmicron() {} // expected-error{{invalid redeclaration of 'dupOmicron()'}}
}

func bracesWsPreservedOnBump() {
  // expected-error@+1 2 {{cannot find 'unboundEta' in scope}}
  unboundEta = 1; unboundEta = 1
}

func multiBlockTailPreserved() {
  unboundTheta = 1 // expected-error{{cannot find 'unboundTheta' in scope}}{{none}}
  unboundIota = 1 // expected-error{{cannot find 'unboundIota' in scope}}
}

func multiBlockTailSpaced() {
  unboundMu = 1 // expected-error{{cannot find 'unboundMu' in scope}}  {{none}}
  unboundNu = 1 // expected-error{{cannot find 'unboundNu' in scope}}
}

@available(*, deprecated, message: "see \\junk")
func deprecatedKappa() {}

func escapesInMessagePreserved() {
  deprecatedKappa() // expected-warning{{'deprecatedKappa()' is deprecated: see \\junk}}
  unboundLambda = 1 // expected-error{{cannot find 'unboundLambda' in scope}}
}

func messageChangeWithBackwardTarget() {
  unboundOmega = 1
  // expected-error @-1 {{cannot find 'unboundOmega' in scope}}
}

func colOnlyTarget() {
  unboundPi = 1 // expected-error@:3{{cannot find 'unboundPi' in scope}}
}

