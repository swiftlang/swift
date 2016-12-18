// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift -emit-sil -emit-module-path %t/SwiftLib.swiftmodule -I %S/Inputs/conformance-removed/ %S/Inputs/conformance-removed/SwiftLib.swift -Xcc -DUSE_PROTO
// RUN: not --crash %target-build-swift -typecheck -I %t -I %S/Inputs/custom-modules/ %s 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

import SwiftLib
class Rdar28282310: Sub {}
// CHECK: If you're seeing a crash here, check that your SDK and dependencies are at least as new as the versions used to build 'SwiftLib'
