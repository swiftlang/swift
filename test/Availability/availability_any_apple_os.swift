// RUN: %target-typecheck-verify-swift -parse-stdlib -target %target-cpu-apple-macos26 -verify-additional-prefix apple- -verify-additional-prefix macos-
// RUN: %target-typecheck-verify-swift -parse-stdlib -target arm64-apple-ios26 -verify-additional-prefix apple- -verify-additional-prefix ios-
// RUN: %target-typecheck-verify-swift -parse-stdlib -target %target-cpu-apple-watchos26 -verify-additional-prefix apple- -verify-additional-prefix watchos-
// RUN: %target-typecheck-verify-swift -parse-stdlib -target %target-cpu-apple-tvos26 -verify-additional-prefix apple- -verify-additional-prefix tvos-
// RUN: %empty-directory(%t/XROS.sdk)
// RUN: cp %S/../ClangImporter/Inputs/XROS26.0.sdk/SDKSettings.json %t/XROS.sdk/
// RUN: %target-typecheck-verify-swift -parse-stdlib -target %target-cpu-apple-visionos26 -sdk %t/XROS.sdk -verify-additional-prefix apple- -verify-additional-prefix visionos-
// RUN: %target-typecheck-verify-swift -parse-stdlib -target x86_64-unknown-linux-gnu
// RUN: %target-typecheck-verify-swift -parse-stdlib -target x86_64-unknown-windows-msvc

// expected-warning@<unknown> * {{libc not found for }}
// expected-warning@<unknown> * {{using sysroot for }}

@available(anyAppleOS 26.1, *)
func availableInAnyAppleOS26_1() { }

@available(anyAppleOS, deprecated: 26)
func deprecatedInAnyAppleOS26() { }

@available(anyAppleOS, obsoleted: 26)
func obsoletedInAnyAppleOS26() { }
// expected-macos-note@-2 {{'obsoletedInAnyAppleOS26()' was obsoleted in macOS 26}}
// expected-ios-note@-3 {{'obsoletedInAnyAppleOS26()' was obsoleted in iOS 26}}
// expected-watchos-note@-4 {{'obsoletedInAnyAppleOS26()' was obsoleted in watchOS 26}}
// expected-tvos-note@-5 {{'obsoletedInAnyAppleOS26()' was obsoleted in tvOS 26}}
// expected-visionos-note@-6 {{'obsoletedInAnyAppleOS26()' was obsoleted in visionOS 26}}

@available(anyAppleOS 26, macOS 26.1, *)
func availableInAnyAppleOS26AndMacOS26_1() { }

@available(macOS 26.1, anyAppleOS 26, *)
func availableInMacOS26_1AndAnyAppleOS26() { }

@available(macOS 26.1, iOS 26.1, watchOS 26.1, tvOS 26.1, visionOS 26.1, *)
func availableInEveryAppleOS26_1() { }

@available(macOS, unavailable)
@available(iOS, unavailable)
@available(watchOS, unavailable)
@available(tvOS, unavailable)
@available(visionOS, unavailable)
func unavailableInEveryAppleOS() {
  availableInAnyAppleOS26_1()
  availableInMacOS26_1AndAnyAppleOS26()
  availableInEveryAppleOS26_1()
  unavailableInAnyAppleOS()
}

@available(anyAppleOS, unavailable) // expected-apple-note {{'unavailableInAnyAppleOS()' has been explicitly marked unavailable here}}
func unavailableInAnyAppleOS() {
  availableInAnyAppleOS26_1()
  availableInMacOS26_1AndAnyAppleOS26()
  availableInEveryAppleOS26_1()
  unavailableInEveryAppleOS()
}

func availableAtDeploymentTarget() {
  // expected-apple-note@-1 {{add '@available' attribute to enclosing global function}}{{1-1=@available(anyAppleOS 26.1, *)\n}}
  // expected-macos-note@-2 2 {{add '@available' attribute to enclosing global function}}{{1-1=@available(macOS 26.1, *)\n}}

  availableInAnyAppleOS26_1()
  // expected-macos-error@-1 {{'availableInAnyAppleOS26_1()' is only available in macOS 26.1 or newer}}
  // expected-ios-error@-2 {{'availableInAnyAppleOS26_1()' is only available in iOS 26.1 or newer}}
  // expected-watchos-error@-3 {{'availableInAnyAppleOS26_1()' is only available in watchOS 26.1 or newer}}
  // expected-tvos-error@-4 {{'availableInAnyAppleOS26_1()' is only available in tvOS 26.1 or newer}}
  // expected-visionos-error@-5 {{'availableInAnyAppleOS26_1()' is only available in visionOS 26.1 or newer}}
  // expected-apple-note@-6 {{add 'if #available' version check}}{{3-30=if #available(anyAppleOS 26.1, *) {\n      availableInAnyAppleOS26_1()\n  \} else {\n      // Fallback on earlier versions\n  \}}}

  deprecatedInAnyAppleOS26()
  // expected-macos-warning@-1 {{'deprecatedInAnyAppleOS26()' was deprecated in macOS 26}}
  // expected-ios-warning@-2 {{'deprecatedInAnyAppleOS26()' was deprecated in iOS 26}}
  // expected-watchos-warning@-3 {{'deprecatedInAnyAppleOS26()' was deprecated in watchOS 26}}
  // expected-tvos-warning@-4 {{'deprecatedInAnyAppleOS26()' was deprecated in tvOS 26}}
  // expected-visionos-warning@-5 {{'deprecatedInAnyAppleOS26()' was deprecated in visionOS 26}}

  obsoletedInAnyAppleOS26()
  // expected-macos-error@-1 {{'obsoletedInAnyAppleOS26()' is unavailable in macOS}}
  // expected-ios-error@-2 {{'obsoletedInAnyAppleOS26()' is unavailable in iOS}}
  // expected-watchos-error@-3 {{'obsoletedInAnyAppleOS26()' is unavailable in watchOS}}
  // expected-tvos-error@-4 {{'obsoletedInAnyAppleOS26()' is unavailable in tvOS}}
  // expected-visionos-error@-5 {{'obsoletedInAnyAppleOS26()' is unavailable in visionOS}}

  availableInAnyAppleOS26AndMacOS26_1()
  // expected-macos-error@-1 {{'availableInAnyAppleOS26AndMacOS26_1()' is only available in macOS 26.1 or newer}}
  // expected-macos-note@-2 {{add 'if #available' version check}}{{3-40=if #available(macOS 26.1, *) {\n      availableInAnyAppleOS26AndMacOS26_1()\n  \} else {\n      // Fallback on earlier versions\n  \}}}
  availableInMacOS26_1AndAnyAppleOS26()
  // expected-macos-error@-1 {{'availableInMacOS26_1AndAnyAppleOS26()' is only available in macOS 26.1 or newer}}
  // expected-macos-note@-2 {{add 'if #available' version check}}{{3-40=if #available(macOS 26.1, *) {\n      availableInMacOS26_1AndAnyAppleOS26()\n  \} else {\n      // Fallback on earlier versions\n  \}}}

  unavailableInAnyAppleOS()
  // expected-macos-error@-1 {{'unavailableInAnyAppleOS()' is unavailable in macOS}}
  // expected-ios-error@-2 {{'unavailableInAnyAppleOS()' is unavailable in iOS}}
  // expected-watchos-error@-3 {{'unavailableInAnyAppleOS()' is unavailable in watchOS}}
  // expected-tvos-error@-4 {{'unavailableInAnyAppleOS()' is unavailable in tvOS}}
  // expected-visionos-error@-5 {{'unavailableInAnyAppleOS()' is unavailable in visionOS}}

  if #available(anyAppleOS 25, *) { } // expected-warning {{'25' is not a valid version number for any Apple OS}}

  if #available(anyAppleOS 26.1, *) {
    availableInAnyAppleOS26_1()
    availableInEveryAppleOS26_1()
    availableInAnyAppleOS26AndMacOS26_1()
    availableInMacOS26_1AndAnyAppleOS26()
  }
  if #available(macOS 26.1, *) {
    availableInAnyAppleOS26AndMacOS26_1()
    availableInMacOS26_1AndAnyAppleOS26()
  }
  if #available(macOS 26.1, iOS 26.1, watchOS 26.1, tvOS 26.1, visionOS 26.1, *) {
    availableInAnyAppleOS26_1()
    availableInEveryAppleOS26_1()
  }
}

@available(anyAppleOS 26.1, *)
struct AvailableInAnyAppleOS26_1 {
  func method() {
    availableInAnyAppleOS26_1()
  }
}

@available(anyAppleOS, unavailable)
func alsoUnavailableInAnyAppleOS() {
  unavailableInAnyAppleOS()
}

class BaseWithDeprecatedInit {
  @available(anyAppleOS, deprecated: 26)
  init() {}
}

@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
class SubclassInheritingDeprecatedInit: BaseWithDeprecatedInit {}

func testInheritedDesignatedInit() {
  // The inherited initializer must still be deprecated even though the
  // subclass has attributes for more specific platforms.
  let _ = SubclassInheritingDeprecatedInit()
  // expected-macos-warning@-1 {{'init()' was deprecated in macOS 26}}
  // expected-ios-warning@-2 {{'init()' was deprecated in iOS 26}}
  // expected-watchos-warning@-3 {{'init()' was deprecated in watchOS 26}}
  // expected-tvos-warning@-4 {{'init()' was deprecated in tvOS 26}}
  // expected-visionos-warning@-5 {{'init()' was deprecated in visionOS 26}}
}

class BaseWithAnyAppleOSInits {
  @available(anyAppleOS 26.1, *)
  init(introduced: ()) {}

  @available(anyAppleOS, obsoleted: 26.1)
  init(obsoleted: ()) {}
}

// The SDK doesn't map iOS 26.1 to visionOS, so the anyAppleOS versions
// inherited by the iOS availability of the initializers must not be remapped.
@available(iOS 13, *)
class SubclassInheritingAnyAppleOSInits: BaseWithAnyAppleOSInits {}

@available(macOS, deprecated: 26)
class BaseDeprecatedInMacOS {
  @available(anyAppleOS 26, *)
  init(x: ()) {}
}

class SubclassOfBaseDeprecatedInMacOS: BaseDeprecatedInMacOS {}
// expected-macos-warning@-1 {{'BaseDeprecatedInMacOS' was deprecated in macOS 26}}

func testInheritedDesignatedInitsAtDeploymentTarget() {
  // expected-apple-note@-1 {{add '@available' attribute to enclosing global function}}

  let _ = SubclassInheritingAnyAppleOSInits(introduced: ())
  // expected-macos-error@-1 {{'init(introduced:)' is only available in macOS 26.1 or newer}}
  // expected-ios-error@-2 {{'init(introduced:)' is only available in iOS 26.1 or newer}}
  // expected-watchos-error@-3 {{'init(introduced:)' is only available in watchOS 26.1 or newer}}
  // expected-tvos-error@-4 {{'init(introduced:)' is only available in tvOS 26.1 or newer}}
  // expected-visionos-error@-5 {{'init(introduced:)' is only available in visionOS 26.1 or newer}}
  // expected-apple-note@-6 {{add 'if #available' version check}}

  let _ = SubclassInheritingAnyAppleOSInits(obsoleted: ())

  // The anyAppleOS availability of the initializer doesn't override the macOS
  // deprecation of its class.
  let _ = SubclassOfBaseDeprecatedInMacOS(x: ())
  // expected-macos-warning@-1 {{'init(x:)' was deprecated in macOS 26}}
}

@available(anyAppleOS, deprecated: 26)
struct DeprecatedInAnyAppleOS {
  @available(macOS 26, iOS 26, watchOS 26, tvOS 26, visionOS 26, *)
  var modifiable: () {
    get { () }
    _modify {
      var value: () = ()
      yield &value
    }
  }
}

func takesInout(_ x: inout ()) { }

func testSetterSynthesizedFromModify(_ s: inout DeprecatedInAnyAppleOS) {
  // expected-macos-warning@-1 {{'DeprecatedInAnyAppleOS' was deprecated in macOS 26}}
  // expected-ios-warning@-2 {{'DeprecatedInAnyAppleOS' was deprecated in iOS 26}}
  // expected-watchos-warning@-3 {{'DeprecatedInAnyAppleOS' was deprecated in watchOS 26}}
  // expected-tvos-warning@-4 {{'DeprecatedInAnyAppleOS' was deprecated in tvOS 26}}
  // expected-visionos-warning@-5 {{'DeprecatedInAnyAppleOS' was deprecated in visionOS 26}}

  // The synthesized setter is deprecated like its struct, even though the
  // property has attributes for more specific platforms.
  takesInout(&s.modifiable)
  // expected-macos-warning@-1 {{setter for 'modifiable' was deprecated in macOS 26}}
  // expected-ios-warning@-2 {{setter for 'modifiable' was deprecated in iOS 26}}
  // expected-watchos-warning@-3 {{setter for 'modifiable' was deprecated in watchOS 26}}
  // expected-tvos-warning@-4 {{setter for 'modifiable' was deprecated in tvOS 26}}
  // expected-visionos-warning@-5 {{setter for 'modifiable' was deprecated in visionOS 26}}
}
