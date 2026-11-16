// Check that a Swift module dependency reports the same link libraries whether
// it is resolved from a textual interface or from a binary module.

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/binary/Fw.framework/Modules/Fw.swiftmodule)
// RUN: %empty-directory(%t/interface/Fw.framework/Modules/Fw.swiftmodule)

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Lib \
// RUN:   -module-link-name LibLink -enable-library-evolution -swift-version 5 \
// RUN:   -o %t/binary/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/interface/Lib.swiftinterface
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name StaticLib \
// RUN:   -module-link-name StaticLibLink -static -enable-library-evolution -swift-version 5 \
// RUN:   -o %t/binary/StaticLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/interface/StaticLib.swiftinterface
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Fw \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -o %t/binary/Fw.framework/Modules/Fw.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/interface/Fw.framework/Modules/Fw.swiftmodule/%target-swiftinterface-name

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache \
// RUN:   %t/main.swift -o %t/interface.json -I %t/interface -F %t/interface
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache \
// RUN:   %t/main.swift -o %t/binary.json -I %t/binary -F %t/binary
// RUN: %validate-json %t/interface.json > %t/interface.formatted.json
// RUN: %validate-json %t/binary.json > %t/binary.formatted.json

// RUN: %FileCheck %s --check-prefixes=LIB,LIB-INTERFACE < %t/interface.formatted.json
// RUN: %FileCheck %s --check-prefixes=LIB,LIB-BINARY < %t/binary.formatted.json
// RUN: %FileCheck %s --check-prefixes=STATIC,STATIC-INTERFACE < %t/interface.formatted.json
// RUN: %FileCheck %s --check-prefixes=STATIC,STATIC-BINARY < %t/binary.formatted.json
// RUN: %FileCheck %s --check-prefixes=FW,FW-INTERFACE < %t/interface.formatted.json
// RUN: %FileCheck %s --check-prefixes=FW,FW-BINARY < %t/binary.formatted.json

// LIB-INTERFACE-LABEL: "modulePath": "{{.*}}{{/|\\}}Lib-{{.*}}.swiftmodule",
// LIB-BINARY-LABEL: "modulePath": "{{.*}}{{/|\\}}binary{{/|\\}}Lib.swiftmodule",
// LIB: "linkLibraries": [
// LIB-NEXT: {
// LIB-NEXT:   "linkName": "LibLink",
// LIB-NEXT:   "isStatic": false,
// LIB-NEXT:   "isFramework": false,
// LIB-NEXT:   "shouldForceLoad": false
// LIB-NEXT: }
// LIB-NEXT: ],

// STATIC-INTERFACE-LABEL: "modulePath": "{{.*}}{{/|\\}}StaticLib-{{.*}}.swiftmodule",
// STATIC-BINARY-LABEL: "modulePath": "{{.*}}{{/|\\}}binary{{/|\\}}StaticLib.swiftmodule",
// STATIC: "linkLibraries": [
// STATIC-NEXT: {
// STATIC-NEXT:   "linkName": "StaticLibLink",
// STATIC-NEXT:   "isStatic": true,
// STATIC-NEXT:   "isFramework": false,
// STATIC-NEXT:   "shouldForceLoad": false
// STATIC-NEXT: }
// STATIC-NEXT: ],

// FW-INTERFACE-LABEL: "modulePath": "{{.*}}{{/|\\}}Fw-{{.*}}.swiftmodule",
// FW-BINARY-LABEL: "modulePath": "{{.*}}{{/|\\}}binary{{/|\\}}Fw.framework{{/|\\}}Modules{{/|\\}}Fw.swiftmodule{{/|\\}}{{.*}}.swiftmodule",
// FW: "linkLibraries": [
// FW-NEXT: {
// FW-NEXT:   "linkName": "Fw",
// FW-NEXT:   "isStatic": false,
// FW-NEXT:   "isFramework": true,
// FW-NEXT:   "shouldForceLoad": false
// FW-NEXT: }
// FW-NEXT: ],

//--- Lib.swift
public func f() {}

//--- main.swift
import Lib
import StaticLib
import Fw
