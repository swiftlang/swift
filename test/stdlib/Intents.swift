// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out3 -swift-version 3 && %target-run %t/a.out3
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-run %t/a.out4
// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: OS=tvos

import Intents
import StdlibUnittest

let IntentsTestSuite = TestSuite("Intents")

#if swift(>=4)
let swiftVersion = "4"
#else
let swiftVersion = "3"
#endif

if #available(OSX 10.12, iOS 10.0, watchOS 3.2, *) {

  IntentsTestSuite.test("ErrorDomain/\(swiftVersion)") {
    expectEqual("IntentsErrorDomain", INIntentErrorDomain)
  }

  IntentsTestSuite.test("extension/\(swiftVersion)") {
    expectEqual("IntentsErrorDomain", INIntentError.errorDomain)
  }
}

#if os(iOS)
if #available(iOS 11.0, *) {

  IntentsTestSuite.test("INParameter KeyPath/\(swiftVersion)") {
    let param = INParameter(keyPath: \INRequestRideIntent.pickupLocation)
    expectEqual("pickupLocation", param?.parameterKeyPath)
    if let typ = param?.parameterClass {
      expectEqual(INRequestRideIntent.self, typ)
    }
    else {
      expectUnreachable()
    }
  }

  IntentsTestSuite.test("INSetProfileInCarIntent/defaultProfile availability/\(swiftVersion)") {
    func f(profile: INSetProfileInCarIntent) {
      var isDefaultProfile = profile.isDefaultProfile
      expectType(Bool?.self, &isDefaultProfile)
#if !swift(>=4)
      var defaultProfile = profile.defaultProfile
      expectType(Int?.self, &defaultProfile)
#endif
    }
  }
}
#endif

#if os(iOS) || os(watchOS)
if #available(iOS 10.0, watchOS 3.2, *) {

  IntentsTestSuite.test("INRideOption usesMeteredFare/\(swiftVersion)") {
    func f(rideOption: inout INRideOption) {
#if swift(>=4)
      rideOption.usesMeteredFare = true
      expectType(Bool?.self, &rideOption.usesMeteredFare)
      expectTrue(rideOption.usesMeteredFare ?? false)
#else
      rideOption.usesMeteredFare = NSNumber(value: true)
      expectType(NSNumber?.self, &rideOption.usesMeteredFare)
      expectTrue(rideOption.usesMeteredFare?.boolValue ?? false)
#endif
    }
  }

}
#endif

#if os(iOS)
if #available(iOS 11.0, *) {
  IntentsTestSuite.test("INSetProfileInCarIntent Initializers/\(swiftVersion)") {
#if swift(>=4)
    _ = INSetProfileInCarIntent()
    _ = INSetProfileInCarIntent(isDefaultProfile: nil)
    _ = INSetProfileInCarIntent(profileName: nil)
    _ = INSetProfileInCarIntent(profileName: nil, isDefaultProfile: nil)
    _ = INSetProfileInCarIntent(profileNumber: nil)
    _ = INSetProfileInCarIntent(profileNumber: nil, isDefaultProfile: nil)
    _ = INSetProfileInCarIntent(profileNumber: nil, profileName: nil)
    _ = INSetProfileInCarIntent(
      profileNumber: nil, profileName: nil, isDefaultProfile: nil)
#else
    _ = INSetProfileInCarIntent()
    _ = INSetProfileInCarIntent(defaultProfile: nil)
    _ = INSetProfileInCarIntent(profileName: nil)
    _ = INSetProfileInCarIntent(profileName: nil, defaultProfile: nil)
    _ = INSetProfileInCarIntent(profileNumber: nil)
    _ = INSetProfileInCarIntent(profileNumber: nil, defaultProfile: nil)
    _ = INSetProfileInCarIntent(profileNumber: nil, profileName: nil)
    _ = INSetProfileInCarIntent(
      profileNumber: nil, profileName: nil, defaultProfile: nil)
#endif
  }
}
#endif

#if os(iOS) || os(watchOS)
if #available(iOS 12.0, watchOS 5.0, *) {
    
  IntentsTestSuite.test("INPlayMediaIntent Initializer/\(swiftVersion)") {
    let intent = INPlayMediaIntent(mediaItems: nil, mediaContainer: nil, playShuffled: false, playbackRepeatMode: .unknown, resumePlayback: true)
    expectFalse(intent.playShuffled ?? true)
    expectTrue(intent.resumePlayback ?? false)
  }
    
}
#endif

runAllTests()
