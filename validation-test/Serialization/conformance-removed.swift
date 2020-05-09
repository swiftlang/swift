// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -emit-module-path %t/SwiftLib.swiftmodule -I %S/Inputs/conformance-removed/ %S/Inputs/conformance-removed/SwiftLib.swift -Xcc -DUSE_PROTO
// RUN: not grep SomeProto %t/SwiftLib.swiftmodule
// RUN: %target-build-swift -typecheck -I %t -I %S/Inputs/custom-modules/ %s

// REQUIRES: objc_interop

import SwiftLib
class Rdar28282310: Sub {}
