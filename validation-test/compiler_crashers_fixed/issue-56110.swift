// RUN: %target-typecheck-verify-swift -disable-objc-interop -module-name Foundation

// https://github.com/apple/swift/issues/56110

// Make sure we diagnose this even if the current module is named 'Foundation':
@objc protocol Horse { // expected-error {{Objective-C interoperability is disabled}}
  func ride()
}

func rideHorse(_ horse: Horse) {
  horse.ride()
}
