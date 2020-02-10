// RUN: %target-build-swift -emit-module %s

// REQUIRES: objc_interop
import Foundation

/// Set of notifications that can be sent by this manager
extension Notification.Name {
    /// Notifications that can be posted by the BLE code.
    class BLE {
        /// A notification posted when a new device has been discovered while scanning.
        static var discoveredDevice = Notification.Name("my_unique_notification name")
    }
}
