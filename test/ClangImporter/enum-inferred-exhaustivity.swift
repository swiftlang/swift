// RUN: %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/enum-inferred-exhaustivity.h -verify -enable-nonfrozen-enum-exhaustivity-diagnostics -warnings-as-errors

// This is testing what happens with a CF_ENUM definition that doesn't include
// any enum_extensibility attributes. As such, the test deliberately avoids
// importing anything that might pull in CoreFoundation, even from the mock SDK.

func test(_ value: EnumWithDefaultExhaustivity) {
  // We want to assume such enums are non-frozen.
  switch value { // expected-error {{switch must be exhaustive}} expected-note {{handle unknown values using "@unknown default"}}
  case .loneCase: break
  }
}

func test(_ value: EnumWithSpecialAttributes) {
  // Same, but with the attributes macro shipped in the Xcode 9 SDKs.
  switch value { // expected-error {{switch must be exhaustive}} expected-note {{handle unknown values using "@unknown default"}}
  case .loneCase: break
  }
}
