// RUN: %target-swift-emit-ir -I %S/Inputs -cxx-interoperability-mode=default %s

// REQUIRES: objc_interop

import ID

let v = getHasIdMember()
