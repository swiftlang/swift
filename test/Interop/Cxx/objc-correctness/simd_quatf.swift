// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default %s

// REQUIRES: objc_interop
// REQUIRES: VENDOR=apple

import simd

var _: simd.simd_quatf! = nil
