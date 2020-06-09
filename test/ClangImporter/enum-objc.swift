// RUN: %target-swift-frontend -emit-sil %s -enable-objc-interop -import-objc-header %S/Inputs/enum-objc.h -verify -enable-nonfrozen-enum-exhaustivity-diagnostics

// REQUIRES: objc_interop

func test(_ value: SwiftEnum, _ exhaustiveValue: ExhaustiveEnum) {
  switch value { // expected-warning {{switch covers known cases, but 'SwiftEnum' may have additional unknown values}} expected-note {{handle unknown values using "@unknown default"}}
  case .one: break
  case .two: break
  case .three: break
  }

  switch exhaustiveValue { // ok
  case .one: break
  case .two: break
  case .three: break
  }
}

let _: Int = forwardBarePointer // expected-error {{cannot convert value of type '(OpaquePointer) -> Void' to specified type 'Int'}}
let _: Int = forwardWithUnderlyingPointer // expected-error {{cannot convert value of type '(OpaquePointer) -> Void' to specified type 'Int'}}
let _: Int = forwardObjCPointer // expected-error {{cannot convert value of type '(OpaquePointer) -> Void' to specified type 'Int'}}

// FIXME: It would be nice to import these as unavailable somehow instead.
let _: Int = forwardWithUnderlyingValue // expected-error {{cannot find 'forwardWithUnderlyingValue' in scope}}
let _: Int = forwardObjCValue // expected-error {{cannot find 'forwardObjCValue' in scope}}

// Note that if /these/ start getting imported as unavailable, the error will
// also mention that there's a missing argument, since the second argument isn't
// actually defaultable.
_ = SomeClass.tryInferDefaultArgumentUnderlyingValue(false) // expected-error {{type 'SomeClass' has no member 'tryInferDefaultArgumentUnderlyingValue'}}
_ = SomeClass.tryInferDefaultArgumentObjCValue(false) // expected-error {{type 'SomeClass' has no member 'tryInferDefaultArgumentObjCValue'}}
