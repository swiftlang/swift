// RUN: %target-typecheck-verify-swift -Wwarning UseAnyAppleOSAvailability \
// RUN:   -define-availability 'FourOSesAligned 26:macOS 26, iOS 26, tvOS 26, watchOS 26' \
// RUN:   -verify-ignore-unrelated

@available(macOS 26, iOS 26, tvOS 26, watchOS 26, *)
// expected-note@-1 {{'allFourOSesAligned()' is available in macOS 26 or newer}}
// expected-note@-2 {{'allFourOSesAligned()' is available in iOS 26 or newer}}
// expected-note@-3 {{'allFourOSesAligned()' is available in tvOS 26 or newer}}
// expected-note@-4 {{'allFourOSesAligned()' is available in watchOS 26 or newer}}
func allFourOSesAligned() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-5:12-52=anyAppleOS 26, *}}

@available(macOS 26.0, iOS 26.0, tvOS 26.0, watchOS 26.0, *)
// expected-note@-1 {{'allFourOSesAlignedTrailingZeroes()' is available in macOS 26.0 or newer}}
// expected-note@-2 {{'allFourOSesAlignedTrailingZeroes()' is available in iOS 26.0 or newer}}
// expected-note@-3 {{'allFourOSesAlignedTrailingZeroes()' is available in tvOS 26.0 or newer}}
// expected-note@-4 {{'allFourOSesAlignedTrailingZeroes()' is available in watchOS 26.0 or newer}}
func allFourOSesAlignedTrailingZeroes() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-5:12-60=anyAppleOS 26, *}}

@available(watchOS 26, tvOS 26, iOS 26, macOS 26, *)
// expected-note@-1 {{'allFourOSesAlignedReversed()' is available in macOS 26 or newer}}
// expected-note@-2 {{'allFourOSesAlignedReversed()' is available in iOS 26 or newer}}
// expected-note@-3 {{'allFourOSesAlignedReversed()' is available in tvOS 26 or newer}}
// expected-note@-4 {{'allFourOSesAlignedReversed()' is available in watchOS 26 or newer}}
func allFourOSesAlignedReversed() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-5:12-52=anyAppleOS 26, *}}

@available(macOS 26, iOS 26.0, tvOS 26, watchOS 26.0, *)
// expected-note@-1 {{'allFourOSesAlignedSomeTrailingZeroes()' is available in macOS 26 or newer}}
// expected-note@-2 {{'allFourOSesAlignedSomeTrailingZeroes()' is available in iOS 26.0 or newer}}
// expected-note@-3 {{'allFourOSesAlignedSomeTrailingZeroes()' is available in tvOS 26 or newer}}
// expected-note@-4 {{'allFourOSesAlignedSomeTrailingZeroes()' is available in watchOS 26.0 or newer}}
func allFourOSesAlignedSomeTrailingZeroes() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-5:12-56=anyAppleOS 26, *}}

@available(FourOSesAligned 26, *)
func allFourOSesAlignedMacro() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}

@available(macOS 26, iOS 26, tvOS 26, watchOS 26, visionOS 26, macCatalyst 26, *)
// expected-note@-1 {{'allSixOSesAligned()' is available in macOS 26 or newer}}
// expected-note@-2 {{'allSixOSesAligned()' is available in iOS 26 or newer}}
// expected-note@-3 {{'allSixOSesAligned()' is available in tvOS 26 or newer}}
// expected-note@-4 {{'allSixOSesAligned()' is available in watchOS 26 or newer}}
// expected-note@-5 {{'allSixOSesAligned()' is available in visionOS 26 or newer}}
// expected-note@-6 {{'allSixOSesAligned()' is available in Mac Catalyst 26 or newer}}
func allSixOSesAligned() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-7:12-81=anyAppleOS 26, *}}

@available(macOS 26.4, iOS 26.4, tvOS 26.4, watchOS 26.4, *)
// expected-note@-1 {{'allFourOSesAlignedMinorVersion()' is available in macOS 26.4 or newer}}
// expected-note@-2 {{'allFourOSesAlignedMinorVersion()' is available in iOS 26.4 or newer}}
// expected-note@-3 {{'allFourOSesAlignedMinorVersion()' is available in tvOS 26.4 or newer}}
// expected-note@-4 {{'allFourOSesAlignedMinorVersion()' is available in watchOS 26.4 or newer}}
func allFourOSesAlignedMinorVersion() { } // expected-warning {{use '@available(anyAppleOS 26.4, *)' instead of platform specific '@available' attributes with the same version}}{{-5:12-60=anyAppleOS 26.4, *}}

@available(macOS 26, iOS 26, tvOS 26, *)
// expected-note@-1 {{'onlyThreeOSes()' is available in macOS 26 or newer}}
// expected-note@-2 {{'onlyThreeOSes()' is available in iOS 26 or newer}}
// expected-note@-3 {{'onlyThreeOSes()' is available in tvOS 26 or newer}}
func onlyThreeOSes() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-4:12-40=anyAppleOS 26, *}}

@available(macOS 26, iOS 26, *)
// expected-note@-1 {{'onlyTwoOSes()' is available in macOS 26 or newer}}
// expected-note@-2 {{'onlyTwoOSes()' is available in iOS 26 or newer}}
func onlyTwoOSes() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-3:12-31=anyAppleOS 26, *}}

@available(macOS 26, *, iOS 26)
// expected-note@-1 {{'onlyTwoOSesWeirdOrder()' is available in macOS 26 or newer}}
// expected-note@-2 {{'onlyTwoOSesWeirdOrder()' is available in iOS 26 or newer}}
func onlyTwoOSesWeirdOrder() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-3:12-31=anyAppleOS 26, *}}

@available( macOS 26, iOS 26, * )
// expected-note@-1 {{'onlyTwoOSesWeirdWhitespace()' is available in macOS 26 or newer}}
// expected-note@-2 {{'onlyTwoOSesWeirdWhitespace()' is available in iOS 26 or newer}}
func onlyTwoOSesWeirdWhitespace() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-3:13-33=anyAppleOS 26, *}}

@available(macOS 26, iOS 26, tvOS 26, watchOS 26.4, *)
// expected-note@-1 {{'threeOSesAlignedOneUnaligned()' is available in macOS 26 or newer}}
// expected-note@-2 {{'threeOSesAlignedOneUnaligned()' is available in iOS 26 or newer}}
// expected-note@-3 {{'threeOSesAlignedOneUnaligned()' is available in tvOS 26 or newer}}
func threeOSesAlignedOneUnaligned() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-4:12-54=anyAppleOS 26, watchOS 26.4, *}}

@available(macOS 26, iOS 26, tvOS 26, watchOS 26, visionOS 26.4, *)
// expected-note@-1 {{'fourOSesAlignedOneUnaligned()' is available in macOS 26 or newer}}
// expected-note@-2 {{'fourOSesAlignedOneUnaligned()' is available in iOS 26 or newer}}
// expected-note@-3 {{'fourOSesAlignedOneUnaligned()' is available in tvOS 26 or newer}}
// expected-note@-4 {{'fourOSesAlignedOneUnaligned()' is available in watchOS 26 or newer}}
func fourOSesAlignedOneUnaligned() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-5:12-67=anyAppleOS 26, visionOS 26.4, *}}

@available(visionOS 26.4, macOS 26, iOS 26, *, tvOS 26, watchOS 26)
// expected-note@-1 {{'fourOSesAlignedOneUnalignedWeirdOrder()' is available in macOS 26 or newer}}
// expected-note@-2 {{'fourOSesAlignedOneUnalignedWeirdOrder()' is available in iOS 26 or newer}}
// expected-note@-3 {{'fourOSesAlignedOneUnalignedWeirdOrder()' is available in tvOS 26 or newer}}
// expected-note@-4 {{'fourOSesAlignedOneUnalignedWeirdOrder()' is available in watchOS 26 or newer}}
func fourOSesAlignedOneUnalignedWeirdOrder() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-5:12-67=anyAppleOS 26, visionOS 26.4, *}}

@available(macOS 26, iOS 26, tvOS 26.4, watchOS 26.4, *)
// expected-note@-1 {{'twoAlignedVersions()' is available in macOS 26 or newer}}
// expected-note@-2 {{'twoAlignedVersions()' is available in iOS 26 or newer}}
func twoAlignedVersions() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-3:12-56=anyAppleOS 26, watchOS 26.4, tvOS 26.4, *}}

@available(macOS 26.4, iOS 26.4, tvOS 26, watchOS 26, *)
// expected-note@-1 {{'twoAlignedVersionsSwapped()' is available in tvOS 26 or newer}}
// expected-note@-2 {{'twoAlignedVersionsSwapped()' is available in watchOS 26 or newer}}
func twoAlignedVersionsSwapped() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{-3:12-56=anyAppleOS 26, iOS 26.4, macOS 26.4, *}}

@available(macOS 26, iOS 26, *)
// expected-note@-1 {{'alignedVersionsSplitIntoMultipleGropus()' is available in macOS 26 or newer}}
// expected-note@-2 {{'alignedVersionsSplitIntoMultipleGropus()' is available in iOS 26 or newer}}
@available(tvOS 26, watchOS 26, *)
// expected-note@-1 {{'alignedVersionsSplitIntoMultipleGropus()' is available in tvOS 26 or newer}}
// expected-note@-2 {{'alignedVersionsSplitIntoMultipleGropus()' is available in watchOS 26 or newer}}
func alignedVersionsSplitIntoMultipleGropus() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{none}}

@available(macOS, introduced: 26) // expected-note {{'fourOSesAlignedLongForm()' is available in macOS 26 or newer}}
@available(iOS, introduced: 26) // expected-note {{'fourOSesAlignedLongForm()' is available in iOS 26 or newer}}
@available(tvOS, introduced: 26) // expected-note {{'fourOSesAlignedLongForm()' is available in tvOS 26 or newer}}
@available(watchOS, introduced: 26) // expected-note {{'fourOSesAlignedLongForm()' is available in watchOS 26 or newer}}
func fourOSesAlignedLongForm() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{none}}

@available(macOS, introduced: 26.4) // expected-note {{'fourOSesAlignedLongFormMinorVersion()' is available in macOS 26.4 or newer}}
@available(iOS, introduced: 26.4) // expected-note {{'fourOSesAlignedLongFormMinorVersion()' is available in iOS 26.4 or newer}}
@available(tvOS, introduced: 26.4) // expected-note {{'fourOSesAlignedLongFormMinorVersion()' is available in tvOS 26.4 or newer}}
@available(watchOS, introduced: 26.4) // expected-note {{'fourOSesAlignedLongFormMinorVersion()' is available in watchOS 26.4 or newer}}
func fourOSesAlignedLongFormMinorVersion() { } // expected-warning {{use '@available(anyAppleOS 26.4, *)' instead of platform specific '@available' attributes with the same version}}{{none}}

@available(macOS, introduced: 26.0) // expected-note {{'fourOSesAlignedLongFormTrailingZeroes()' is available in macOS 26.0 or newer}}
@available(iOS, introduced: 26.0) // expected-note {{'fourOSesAlignedLongFormTrailingZeroes()' is available in iOS 26.0 or newer}}
@available(tvOS, introduced: 26.0) // expected-note {{'fourOSesAlignedLongFormTrailingZeroes()' is available in tvOS 26.0 or newer}}
@available(watchOS, introduced: 26.0) // expected-note {{'fourOSesAlignedLongFormTrailingZeroes()' is available in watchOS 26.0 or newer}}
func fourOSesAlignedLongFormTrailingZeroes() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{none}}

@available(macOS, introduced: 26.0) // expected-note {{'fourOSesAlignedLongFormSomeTrailingZeroes()' is available in macOS 26.0 or newer}}
@available(iOS, introduced: 26) // expected-note {{'fourOSesAlignedLongFormSomeTrailingZeroes()' is available in iOS 26 or newer}}
@available(tvOS, introduced: 26.0) // expected-note {{'fourOSesAlignedLongFormSomeTrailingZeroes()' is available in tvOS 26.0 or newer}}
@available(watchOS, introduced: 26) // expected-note {{'fourOSesAlignedLongFormSomeTrailingZeroes()' is available in watchOS 26 or newer}}
func fourOSesAlignedLongFormSomeTrailingZeroes() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{none}}

@available(macOS, introduced: 26) // expected-note {{'fiveOSesAlignedLongForm()' is available in macOS 26 or newer}}
@available(iOS, introduced: 26) // expected-note {{'fiveOSesAlignedLongForm()' is available in iOS 26 or newer}}
@available(tvOS, introduced: 26) // expected-note {{'fiveOSesAlignedLongForm()' is available in tvOS 26 or newer}}
@available(watchOS, introduced: 26) // expected-note {{'fiveOSesAlignedLongForm()' is available in watchOS 26 or newer}}
@available(visionOS, introduced: 26) // expected-note {{'fiveOSesAlignedLongForm()' is available in visionOS 26 or newer}}
func fiveOSesAlignedLongForm() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{none}}

@available(macOS 26, iOS 26, *)
// expected-note@-1 {{'fourOSesAlignedMixedShortFormAndLongForm()' is available in macOS 26 or newer}}
// expected-note@-2 {{'fourOSesAlignedMixedShortFormAndLongForm()' is available in iOS 26 or newer}}
@available(tvOS, introduced: 26) // expected-note {{'fourOSesAlignedMixedShortFormAndLongForm()' is available in tvOS 26 or newer}}
@available(watchOS, introduced: 26) // expected-note {{'fourOSesAlignedMixedShortFormAndLongForm()' is available in watchOS 26 or newer}}
func fourOSesAlignedMixedShortFormAndLongForm() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{none}}

@available(macOS, introduced: 26) // expected-note {{'fourOSesAlignedMixedShortFormAndLongFormSwapped()' is available in macOS 26 or newer}}
@available(iOS, introduced: 26) // expected-note {{'fourOSesAlignedMixedShortFormAndLongFormSwapped()' is available in iOS 26 or newer}}
@available(tvOS 26, watchOS 26, *)
// expected-note@-1 {{'fourOSesAlignedMixedShortFormAndLongFormSwapped()' is available in tvOS 26 or newer}}
// expected-note@-2 {{'fourOSesAlignedMixedShortFormAndLongFormSwapped()' is available in watchOS 26 or newer}}
func fourOSesAlignedMixedShortFormAndLongFormSwapped() { } // expected-warning {{use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version}}{{none}}

@available(macOS, introduced: 26, deprecated: 26.4)
@available(iOS, introduced: 26, deprecated: 26.4)
@available(tvOS, introduced: 26, deprecated: 26.4)
@available(watchOS, introduced: 26, deprecated: 26.4)
func fourOSesAlignedLongFormAndDeprecated() { }

@available(macOS, introduced: 26, obsoleted: 26.4)
@available(iOS, introduced: 26, obsoleted: 26.4)
@available(tvOS, introduced: 26, obsoleted: 26.4)
@available(watchOS, introduced: 26, obsoleted: 26.4)
func fourOSesAlignedLongFormAndObsoleted() { }

@available(macOS, introduced: 26, message: "use something else")
@available(iOS, introduced: 26, message: "use something else")
@available(tvOS, introduced: 26, message: "use something else")
@available(watchOS, introduced: 26, message: "use something else")
func fourOSesAlignedLongFormWithMessage() { }

@available(anyAppleOS 26, *)
func alreadyUsesAnyAppleOS() { }

@available(anyAppleOS 26, watchOS 26.4, tvOS 26.4, *)
func alreadyUsesAnyAppleOSWithExceptions() { }

@available(macOS 26, iOS 26.1, tvOS 26.2, watchOS 26.3, *)
func noCommonVersions() { }

@available(macOS 11, iOS 11, tvOS 11, watchOS 11, *)
func versionsTooOld() { }

@available(macOS, unavailable)
@available(iOS, unavailable)
@available(tvOS, unavailable)
@available(watchOS, unavailable)
func fourOSesAlwaysUnavailable() { }

@available(macOS, deprecated)
@available(iOS, deprecated)
@available(tvOS, deprecated)
@available(watchOS, deprecated)
func fourOSesAlwaysDeprecated() { }

@available(macOSApplicationExtension 26, iOSApplicationExtension 26, tvOSApplicationExtension 26, watchOSApplicationExtension 26, *)
func fourAppExtensionVersions() { }

@available(macOSApplicationExtension, introduced: 26)
@available(iOSApplicationExtension, introduced: 26)
@available(tvOSApplicationExtension, introduced: 26)
@available(watchOSApplicationExtension, introduced: 26)
func fourAppExtensionVersionsLongForm() { }

@available(Windows 26, Android 26, *)
func nonAppleOSesAligned() { }
