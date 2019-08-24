// RUN: %target-build-swift %s

// REQUIRES: objc_interop

// OS X does not have HomeKit.
// UNSUPPORTED: OS=macosx
import HomeKit

if #available(iOS 8.0, watchOS 2.0, tvOS 10.0, *) {
  let s: String = HMCharacteristicPropertySupportsEventNotification
}

