// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -emit-module-path %t/Lib.swiftmodule -module-name Lib -enable-experimental-com-interop -I %t %S/Inputs/metatype-extension-lib.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

// A protocol metatype extension defined in another module round-trips: the
// deserialized extension is recognized as a metatype extension from its
// extended type, and its members are reachable on the protocol metatype.

import COM
import Lib

let _: Int = IWidget.tag
let _: String = IWidget.describe()

// The synthesized IID is still reachable too.
let _: GUID = IWidget.IID

// Declaration classification is reconstructed from the deserialized @com
// attribute, so a client can also extend the imported interface metatype.
extension IWidget.Protocol {
  var clientTag: Int { 8 }
}

let _: Int = IWidget.clientTag
