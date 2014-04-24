// RUN: %target-swift-frontend -parse -verify %s

// Distributed Objects was never available on iOS.
// REQUIRES: OS=macosx

import Foundation
var x : NSConnection  // expected-error {{'NSConnection' is unavailable}}
