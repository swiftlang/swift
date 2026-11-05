// RUN: %target-swift-frontend -typecheck %s -verify
// REQUIRES: OS=linux-android || OS=linux-androideabi

import Android

// The system property API from `sys/system_properties.h` is part of the module.
let _: Int32 = PROP_VALUE_MAX
let _: (UnsafePointer<CChar>, UnsafeMutablePointer<CChar>) -> Int32 = __system_property_get
