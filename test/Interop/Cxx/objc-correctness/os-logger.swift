// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import os

var _: os.Logger! = nil
