// RUN: %swift -typecheck -target i386-apple-watchos2.0 -verify -sdk %sdk %s
// REQUIRES: CPU=i386, OS=watchos
// REQUIRES: libdispatch

import Dispatch

// These are deprecated on all versions of watchOS.
_ = DispatchQueue.GlobalQueuePriority.high // expected-warning {{'high' is deprecated in watchOS: Use qos attributes instead}}
_ = DispatchQueue.GlobalQueuePriority.default // expected-warning {{'default' is deprecated in watchOS: Use qos attributes instead}}
_ = DispatchQueue.GlobalQueuePriority.low // expected-warning {{'low' is deprecated in watchOS: Use qos attributes instead}}
let b = DispatchQueue.GlobalQueuePriority.background // expected-warning {{'background' is deprecated in watchOS: Use qos attributes instead}}

_ = DispatchQueue.global(priority:b)   // expected-warning {{'global(priority:)' is deprecated in watchOS}}
