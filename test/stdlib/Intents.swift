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
if #available(iOS 12.0, watchOS 5.0, *) {

  // helper functions to build test data
  func buildIntent(_ identifier: String) -> INIntent {
    let person = INPerson(personHandle: INPersonHandle(value: "123-456-7890", type: .phoneNumber), nameComponents: nil, displayName: "test person \(identifier)", image: nil, contactIdentifier: nil, customIdentifier: "testPerson \(identifier)")
    return INSendMessageIntent(recipients: [person], content: "test content \(identifier)", speakableGroupName: INSpeakableString(spokenPhrase: "test group \(identifier)"), conversationIdentifier: nil, serviceName: "test service \(identifier)", sender: person)
  }
  func buildUserActivity(_ identifier: String) -> NSUserActivity {
    return NSUserActivity(activityType: "test activity \(identifier)")
  }

  IntentsTestSuite.test("INShortcutOverlay/is enum/\(swiftVersion)") {
    // INIntent
    let originalIntent = buildIntent("A")
    let shortcutWithIntent: INShortcut = .intent(originalIntent)
    switch shortcutWithIntent {
    case .intent(let intent):
      expectEqual(intent, originalIntent)
    case .userActivity:
      expectUnreachable()
    }
    // test convenince properties
    expectEqual(shortcutWithIntent.intent, originalIntent)
    expectEqual(shortcutWithIntent.userActivity, nil)
    // test convenince init
    expectEqual(INShortcut(intent: originalIntent), shortcutWithIntent)

    // NSUserActivity
    let originalUserActivity = buildUserActivity("A")
    let shortcutWithNSUA: INShortcut = .userActivity(originalUserActivity)
    switch shortcutWithNSUA {
    case .intent:
      expectUnreachable()
    case .userActivity(let userActivity):
      expectEqual(userActivity, originalUserActivity)
    }
    // test convenince properties
    expectEqual(shortcutWithNSUA.intent, nil)
    expectEqual(shortcutWithNSUA.userActivity, originalUserActivity)
    // test convenince init
    expectEqual(INShortcut(userActivity: originalUserActivity), shortcutWithNSUA)
  }

  IntentsTestSuite.test("INShortcutOverlay/conformances/\(swiftVersion)") {
    let intentA = buildIntent("A")
    let shortcutIntentA: INShortcut = .intent(intentA)
    let shortcutIntentA2: INShortcut = .intent(intentA)
    let shortcutIntentB: INShortcut = .intent(buildIntent("B"))
    let userActivityA = buildUserActivity("A")
    let shortcutUserActivityA: INShortcut = .userActivity(userActivityA)
    let shortcutUserActivityA2: INShortcut = .userActivity(userActivityA)
    let shortcutUserActivityB: INShortcut = .userActivity(buildUserActivity("B"))

    // Equatable
    expectEqual(shortcutIntentA, shortcutIntentA2)
    expectNotEqual(shortcutIntentA, shortcutIntentB)
    expectEqual(shortcutUserActivityA, shortcutUserActivityA2)
    expectNotEqual(shortcutUserActivityA, shortcutUserActivityB)
    expectNotEqual(shortcutIntentA, shortcutUserActivityA)

    // Hashable
    // expectEqual(shortcutIntentA.hashValue, shortcutIntentA.hashValue)
//     expectEqual(shortcutUserActivityA.hashValue, shortcutUserActivityA.hashValue)

    // Strings
    let _: String = shortcutIntentA.description
    let _: String = shortcutIntentA.debugDescription
  }

  // Make sure the shortcut property of INVoiceShortcut is imported as the overlay enum type
  IntentsTestSuite.test("INShortcutOverlay/INVoiceShortcut propertyIsEnum/\(swiftVersion)") {
    // NOTE: we can't actually run this one becuase we can't easily create an INVoiceShortcut, but at least type-check it
    func f(voiceShortcut: INVoiceShortcut) {
      switch voiceShortcut.shortcut {
      case .intent(let intent):
        print("got intent \(intent)")
      case .userActivity(let userActivity):
        print("got userActivity \(userActivity)")
      }
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
