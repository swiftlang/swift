// Check that the link libraries reported for a Swift module dependency
// resolved to a textual interface match those the binary module built from
// that interface would carry: the '-module-link-name' library (force-loaded
// only with '-autolink-force-load') and the framework itself for framework
// modules.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/Frameworks/FW.framework/Modules/FW.swiftmodule
// RUN: mkdir -p %t/Frameworks/FWLinkName.framework/Modules/FWLinkName.swiftmodule
// RUN: cp %t/inputs/FW.swiftinterface %t/Frameworks/FW.framework/Modules/FW.swiftmodule/%target-swiftinterface-name
// RUN: cp %t/inputs/FWLinkName.swiftinterface %t/Frameworks/FWLinkName.framework/Modules/FWLinkName.swiftmodule/%target-swiftinterface-name

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache \
// RUN:   %t/main.swift -o %t/deps.json -I %t/Swift -F %t/Frameworks
// RUN: %validate-json %t/deps.json > %t/deps.pretty.json
// RUN: %FileCheck %s --check-prefix=LINKNAME < %t/deps.pretty.json
// RUN: %FileCheck %s --check-prefix=FORCELOAD < %t/deps.pretty.json
// RUN: %FileCheck %s --check-prefix=NOLINKNAME < %t/deps.pretty.json
// RUN: %FileCheck %s --check-prefix=FORCELOAD-NOLINKNAME < %t/deps.pretty.json
// RUN: %FileCheck %s --check-prefix=FRAMEWORK < %t/deps.pretty.json
// RUN: %FileCheck %s --check-prefix=FRAMEWORK-LINKNAME < %t/deps.pretty.json

// LINKNAME-LABEL: "modulePath": "{{.*}}clang-module-cache{{[/\\]+}}LinkName-{{.*}}.swiftmodule"
// LINKNAME:       "linkLibraries": [
// LINKNAME-NEXT:    {
// LINKNAME-NEXT:      "linkName": "swiftLinkNameLib",
// LINKNAME-NEXT:      "isStatic": false,
// LINKNAME-NEXT:      "isFramework": false,
// LINKNAME-NEXT:      "shouldForceLoad": false
// LINKNAME-NEXT:    }
// LINKNAME-NEXT:  ],

// FORCELOAD-LABEL: "modulePath": "{{.*}}clang-module-cache{{[/\\]+}}ForceLoad-{{.*}}.swiftmodule"
// FORCELOAD:       "linkLibraries": [
// FORCELOAD-NEXT:    {
// FORCELOAD-NEXT:      "linkName": "swiftForceLoadLib",
// FORCELOAD-NEXT:      "isStatic": false,
// FORCELOAD-NEXT:      "isFramework": false,
// FORCELOAD-NEXT:      "shouldForceLoad": true
// FORCELOAD-NEXT:    }
// FORCELOAD-NEXT:  ],

// NOLINKNAME-LABEL: "modulePath": "{{.*}}clang-module-cache{{[/\\]+}}NoLinkName-{{.*}}.swiftmodule"
// NOLINKNAME:       "linkLibraries": [],

// '-autolink-force-load' has no effect without '-module-link-name'.
// FORCELOAD-NOLINKNAME-LABEL: "modulePath": "{{.*}}clang-module-cache{{[/\\]+}}ForceLoadNoLinkName-{{.*}}.swiftmodule"
// FORCELOAD-NOLINKNAME:       "linkLibraries": [],

// FRAMEWORK-LABEL: "modulePath": "{{.*}}clang-module-cache{{[/\\]+}}FW-{{.*}}.swiftmodule"
// FRAMEWORK:       "linkLibraries": [
// FRAMEWORK-NEXT:    {
// FRAMEWORK-NEXT:      "linkName": "FW",
// FRAMEWORK-NEXT:      "isStatic": false,
// FRAMEWORK-NEXT:      "isFramework": true,
// FRAMEWORK-NEXT:      "shouldForceLoad": false
// FRAMEWORK-NEXT:    }
// FRAMEWORK-NEXT:  ],

// FRAMEWORK-LINKNAME-LABEL: "modulePath": "{{.*}}clang-module-cache{{[/\\]+}}FWLinkName-{{.*}}.swiftmodule"
// FRAMEWORK-LINKNAME:       "linkLibraries": [
// FRAMEWORK-LINKNAME-NEXT:    {
// FRAMEWORK-LINKNAME-NEXT:      "linkName": "swiftFWLinkNameLib",
// FRAMEWORK-LINKNAME-NEXT:      "isStatic": false,
// FRAMEWORK-LINKNAME-NEXT:      "isFramework": false,
// FRAMEWORK-LINKNAME-NEXT:      "shouldForceLoad": true
// FRAMEWORK-LINKNAME-NEXT:    },
// FRAMEWORK-LINKNAME-NEXT:    {
// FRAMEWORK-LINKNAME-NEXT:      "linkName": "FWLinkName",
// FRAMEWORK-LINKNAME-NEXT:      "isStatic": false,
// FRAMEWORK-LINKNAME-NEXT:      "isFramework": true,
// FRAMEWORK-LINKNAME-NEXT:      "shouldForceLoad": false
// FRAMEWORK-LINKNAME-NEXT:    }
// FRAMEWORK-LINKNAME-NEXT:  ],

//--- main.swift
import LinkName
import ForceLoad
import NoLinkName
import ForceLoadNoLinkName
import FW
import FWLinkName

//--- Swift/LinkName.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name LinkName -module-link-name swiftLinkNameLib -parse-stdlib
public func linkName() {}

//--- Swift/ForceLoad.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name ForceLoad -autolink-force-load -module-link-name swiftForceLoadLib -parse-stdlib
public func forceLoad() {}

//--- Swift/NoLinkName.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name NoLinkName -parse-stdlib
public func noLinkName() {}

//--- Swift/ForceLoadNoLinkName.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name ForceLoadNoLinkName -autolink-force-load -parse-stdlib
public func forceLoadNoLinkName() {}

//--- inputs/FW.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name FW -parse-stdlib
public func fw() {}

//--- inputs/FWLinkName.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name FWLinkName -autolink-force-load -module-link-name swiftFWLinkNameLib -parse-stdlib
public func fwLinkName() {}
