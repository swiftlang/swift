// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.9 -typecheck %s -verify -verify-additional-prefix only-available-
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.15 -typecheck %s -verify
// REQUIRES: OS=macosx

protocol P {}
struct X: P {}

func alwaysOpaque() -> some P { return X() } // expected-only-available-error{{'some' return types are only available}} expected-only-available-note{{add '@available'}}

@available(SwiftStdlib 5.1, *)
func sometimesOpaque() -> some P { return X() }

struct NeverConformsToP {}

@available(*, unavailable)
extension NeverConformsToP: P {} // expected-note 2 {{conformance of 'NeverConformsToP' to 'P' has been explicitly marked unavailable here}}

@available(SwiftStdlib 5.1, *)
struct Outer {
  func returnsNeverConformsToP() -> some P {
    NeverConformsToP() // expected-error {{conformance of 'NeverConformsToP' to 'P' is unavailable}}
  }

  @available(macOS, unavailable)
  func returnsNeverConformsToP_macOSUnavailable() -> some P {
    NeverConformsToP() // expected-error {{conformance of 'NeverConformsToP' to 'P' is unavailable}}
  }

  @available(*, unavailable)
  func returnsNeverConformsToP_alwaysUnavailable() -> some P {
    NeverConformsToP()
  }
}

struct ConformsToPExeceptOnMacOS {}

@available(macOS, unavailable)
extension ConformsToPExeceptOnMacOS: P {} // expected-note {{conformance of 'ConformsToPExeceptOnMacOS' to 'P' has been explicitly marked unavailable here}}

@available(SwiftStdlib 5.1, *)
extension Outer {
  func returnsConformsToPExeceptOnMacOS() -> some P {
    ConformsToPExeceptOnMacOS() // expected-error {{conformance of 'ConformsToPExeceptOnMacOS' to 'P' is unavailable in macOS}}
  }

  @available(macOS, unavailable)
  func returnsConformsToPExeceptOnMacOS_macOSUnavailable() -> some P {
    ConformsToPExeceptOnMacOS()
  }

  @available(*, unavailable)
  func returnsConformsToPExeceptOnMacOS_alwaysUnavailable() -> some P {
    ConformsToPExeceptOnMacOS()
  }

}
