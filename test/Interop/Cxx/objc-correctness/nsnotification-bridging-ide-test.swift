// RUN: %target-swift-ide-test -print-module -module-to-print=NSNofiticationBridging -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: @_exported import Foundation

// CHECK: @available(swift, obsoleted: 3, renamed: "NSNotification.Name.SpaceShip")
// CHECK: let SpaceShipNotification: NSNotification.Name
