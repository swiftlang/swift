// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-objc-interop -enable-experimental-cxx-interop
// REQUIRES: objc_interop

import CustomNSOptions

let flags1: MyControlFlags = []
let flags2: MyControlFlags = [.first]
