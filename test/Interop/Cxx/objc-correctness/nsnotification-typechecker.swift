// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-objc-interop -enable-experimental-cxx-interop
// REQUIRES: objc_interop

import Foundation
import NSNotificationBridging

func test(_ n: NSNotification.Name) {}

test(NSNotification.Name.SpaceShip)
test(NSNotification.Name.CExtern)
