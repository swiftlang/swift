// RUN: %swift -typecheck -target i386-apple-watchos2.0 -verify -sdk %sdk %s
// REQUIRES: CPU=i386, OS=watchos
// REQUIRES: objc_interop

import Foundation
import Dispatch

// These are deprecated on all versions of watchOS.
_ = DispatchQueue.GlobalQueuePriority.high // expected-warning {{'high' is deprecated on watchOS: Use qos attributes instead}}
_ = DispatchQueue.GlobalQueuePriority.default // expected-warning {{'default' is deprecated on watchOS: Use qos attributes instead}}
_ = DispatchQueue.GlobalQueuePriority.low // expected-warning {{'low' is deprecated on watchOS: Use qos attributes instead}}
let b = DispatchQueue.GlobalQueuePriority.background // expected-warning {{'background' is deprecated on watchOS: Use qos attributes instead}}

_ = DispatchQueue.global(priority:b)   // expected-warning {{'global(priority:)' is deprecated on watchOS}}
