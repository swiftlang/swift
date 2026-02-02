// RUN: %target-typecheck-verify-swift

@available(macOS, introduced: 2147483646) // expected-warning {{'2147483646' is not a supported version number}}
func funcIntroducedInMacOS2147483646() { }

@available(macOS 2147483646, *) // expected-warning {{'2147483646' is not a supported version number}}
func funcIntroducedInMacOS2147483646Short() { }

@available(macOS, deprecated: 2147483646) // expected-warning {{'2147483646' is not a supported version number}}
func funcDeprecatedInMacOS2147483646() { }

@available(macOS, obsoleted: 2147483646) // expected-warning {{'2147483646' is not a supported version number}}
func funcObsoletedInMacOS2147483646() { }

@available(macOS, introduced: 2147483647) // expected-warning {{'2147483647' is not a supported version number}}
func funcIntroducedInMacOS2147483647() { }

@available(macOS 2147483647, *) // expected-warning {{'2147483647' is not a supported version number}}
func funcIntroducedInMacOS2147483647Short() { }

@available(macOS, deprecated: 2147483647) // expected-warning {{'2147483647' is not a supported version number}}
func funcDeprecatedInMacOS2147483647() { }

@available(macOS, obsoleted: 2147483647) // expected-warning {{'2147483647' is not a supported version number}}
func funcObsoletedInMacOS2147483647() { }

@available(swift, introduced: 2147483646) // expected-warning {{'2147483646' is not a supported version number}}
func funcIntroducedInSwift2147483646() { }

func useExtremeVersions() {
  if #available(macOS 2147483646, *) { // expected-warning {{'2147483646' is not a supported version number}}
    funcIntroducedInMacOS2147483646()
    funcIntroducedInMacOS2147483646Short()
    funcDeprecatedInMacOS2147483646()
    funcObsoletedInMacOS2147483646()
  }
  if #available(macOS 2147483647, *) { // expected-warning {{'2147483647' is not a supported version number}}
    funcIntroducedInMacOS2147483647()
    funcIntroducedInMacOS2147483647Short()
    funcDeprecatedInMacOS2147483647()
    funcObsoletedInMacOS2147483647()
  }
  funcIntroducedInSwift2147483646()
}
