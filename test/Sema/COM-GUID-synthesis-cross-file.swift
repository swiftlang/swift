// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift

// The `@com` interface `IWidget` is defined in the SECONDARY file
// `com_iwidget.swift`; this primary file references `IWidget.IID`.  The IID must
// be synthesized even though the interface lives in a non-primary file.
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -I %t -primary-file %s %S/Inputs/com_iwidget.swift

// Inverse ordering: make `com_iwidget.swift` the primary file and this file the
// secondary one; its own `IWidget.IID` reference must still resolve.
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -I %t %s -primary-file %S/Inputs/com_iwidget.swift

import COM

// Must resolve even though IWidget is defined in a secondary file.
func useWidgetIID() -> GUID { return IWidget.IID }
