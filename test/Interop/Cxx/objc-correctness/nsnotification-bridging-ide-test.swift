// RUN: %target-swift-ide-test -print-module -module-to-print=NSNotificationBridging -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: import Foundation

// CHECK: let SpaceShipNotification: String
// CHECK: let CExternNotification: String
