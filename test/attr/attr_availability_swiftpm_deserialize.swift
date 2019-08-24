// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/PackageDescription.swiftmodule -module-name PackageDescription %S/Inputs/PackageDescription.swift 
// RUN: not %target-swift-frontend -typecheck -I %t -package-description-version 4.2 %s 2>&1 | %FileCheck -check-prefix FOURTWO %s
// RUN: not %target-swift-frontend -typecheck -I %t -package-description-version 5 %s 2>&1 | %FileCheck -check-prefix FIVE %s
// RUN: %target-swift-ide-test -print-module -module-to-print PackageDescription -source-filename x -I %t | %FileCheck %S/Inputs/PackageDescription.swift

import PackageDescription

// FOURTWO: warning: 'v3' is deprecated
// FOURTWO: error: 'v5' is unavailable
// FOURTWO: note: 'v5' was introduced in PackageDescription 5.0
// FIVE: error: 'v3' is unavailable
// FIVE: note: 'v3' was obsoleted in PackageDescription 5.0
let package = Package(
    swiftVersion: [ .v3, .v4, .v5 ]
)

// FOURTWO: note: 'buildSettings' was introduced in PackageDescription 4.3
package.buildSettings = ["Foo": "Bar"]
