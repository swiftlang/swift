// RUN: %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/enum-inferred-exhaustivity.h -swift-version 4 -warnings-as-errors
// RUN: %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/enum-inferred-exhaustivity.h -swift-version 5 -verify

// This is testing what happens with a CF_ENUM definition that doesn't include
// any enum_extensibility attributes. As such, the test deliberately avoids
// importing anything that might pull in CoreFoundation, even from the mock SDK.

func test(_ value: EnumWithDefaultExhaustivity) {
  // In Swift 4 this should be accepted as is, without even getting warnings,
  // until we actually ship Swift 5. Then it should be a warning in Swift 4
  // mode and an error in Swift 5.
  switch value { // expected-error {{switch must be exhaustive}} expected-note {{do you want to add a default clause?}}
  case .loneCase: break
  }
}

func test(_ value: EnumWithSpecialAttributes) {
  // Same, but with the attributes macro shipped in the Xcode 9 SDKs.
  switch value { // expected-error {{switch must be exhaustive}} expected-note {{do you want to add a default clause?}}
  case .loneCase: break
  }
}
