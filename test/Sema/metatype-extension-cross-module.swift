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
