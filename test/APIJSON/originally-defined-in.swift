// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %t/MyModuleCore.swift -parse-as-library  -emit-module -emit-module-path %t/MyModuleCore.swiftmodule -enable-library-evolution -module-name MyModuleCore -swift-version 5 -emit-api-descriptor-path %t/api.json -target arm64-apple-macos26 -library-level api -previous-module-installname-map-file %t/previous-module-installname-map.json
// RUN: %validate-json %t/api.json | %FileCheck %s

//--- MyModuleCore.swift
@_originallyDefinedIn(module: "MyModule", macOS 15)
@available(macOS 13, *)
public class MyClass {}

//--- previous-module-installname-map.json
[
  {
    "module": "MyModule",
    "install_name": "/System/Library/Frameworks/MyModule.framework/MyModule",
    "platforms": ["macOS"]
  }
]

//--- Checks
// CHECK:      {
// CHECK-NEXT:   "target": "arm64-apple-macos26",
// CHECK-NEXT:   "globals": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "$ld$previous$/System/Library/Frameworks/MyModule.framework/MyModule$$1$1.0$15.0$_$s8MyModule0A5ClassCMa$",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "$ld$previous$/System/Library/Frameworks/MyModule.framework/MyModule$$1$1.0$15.0$_$s8MyModule0A5ClassCMm$",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "$ld$previous$/System/Library/Frameworks/MyModule.framework/MyModule$$1$1.0$15.0$_$s8MyModule0A5ClassCMn$",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "$ld$previous$/System/Library/Frameworks/MyModule.framework/MyModule$$1$1.0$15.0$_$s8MyModule0A5ClassCMo$",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "$ld$previous$/System/Library/Frameworks/MyModule.framework/MyModule$$1$1.0$15.0$_$s8MyModule0A5ClassCMu$",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "$ld$previous$/System/Library/Frameworks/MyModule.framework/MyModule$$1$1.0$15.0$_$s8MyModule0A5ClassCN$",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "$ld$previous$/System/Library/Frameworks/MyModule.framework/MyModule$$1$1.0$15.0$_$s8MyModule0A5ClassCfD$",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "$ld$previous$/System/Library/Frameworks/MyModule.framework/MyModule$$1$1.0$15.0$_$s8MyModule0A5ClassCfd$",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCMa",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCMm",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCMn",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCMo",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCMu",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCN",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCfD",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCfd",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "TMP_DIR/MyModuleCore.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "13"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "interfaces": [],
// CHECK-NEXT:   "categories": [],
// CHECK-NEXT:   "version": "1.0"
// CHECK-NEXT: }
