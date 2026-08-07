// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -com-interop-model=microsoft %S/../Inputs/COM.swift

// Build module A (importing COM) which defines the `@com` interface IWidget:
// RUN: %target-swift-frontend -emit-module-path %t/A.swiftmodule -module-name A -enable-experimental-com-interop -com-interop-model=microsoft -I %t %S/Inputs/com_iwidget_moduleA.swift

// Client: import A and use `IWidget.IID` through the builtin conformance of the
// imported interface metatype to `COMInterface`.
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import A

let _: GUID = IWidget.IID
let _: GUID = Widget.CLSID
