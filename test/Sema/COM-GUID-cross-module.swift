// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift

// Build module A (importing COM) which defines the `@com` interface IWidget:
// RUN: %target-swift-frontend -emit-module-path %t/A.swiftmodule -module-name A -enable-experimental-com-interop -I %t %S/Inputs/com_iwidget_moduleA.swift

// Client: import A and use `IWidget.IID`, proving the IID is re-derived in the
// importer from the deserialized `@com` attribute.
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import COM
import A

let _: GUID = IWidget.IID   // re-derived in the importer from the deserialized @com attribute
