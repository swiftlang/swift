// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-objc-interop -enable-experimental-cxx-interop
// REQUIRES: OS=macosx

// Temporarily disable (rdar://128544755)
// REQUIRES: rdar128544755

import Darwin

_ = pthread_mutexattr_t() // expected-warning {{'init()' is deprecated: This zero-initializes the backing memory of the struct}}
