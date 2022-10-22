// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/ShadowFoundation.swiftmodule -module-name ShadowFoundation %S/Inputs/ShadowFoundation.swift -disable-availability-checking
// RUN: %target-typecheck-verify-swift -I %t -disable-availability-checking

import Foundation
import ShadowFoundation

func f(_ uuid: UUID) -> Bool {
    return uuid.uuidString == "FakeUUID"
}

func g(_ uuid: Foundation.UUID) { }